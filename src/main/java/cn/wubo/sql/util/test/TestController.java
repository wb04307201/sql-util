package cn.wubo.sql.util.test;

import cn.wubo.sql.util.ModelSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.SQL;

import java.util.List;

public class TestController {

    public static void main(String[] args) {
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }

        Boolean check = MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.isTableExists(connection));

        if (Boolean.TRUE.equals(check)) {
            MutilConnectionPool.run("test", connection -> new SQL<User>() {
            }.drop().parse().dropTable(connection));
        }

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.create().parse().createTable(connection));

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.insert().addSet("user_name", "aaaa").parse().executeUpdate(connection));

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.update().addSet("user_name", "bbbb").addWhereEQ("user_name", "aaaa").parse().executeUpdate(connection));

        List<User> list = MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.select().addWhereEQ("user_name", "bbbb").parse().executeQuery(connection));
        System.out.println(list);

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.delete().addWhereEQ("user_name", "bbbb").parse().executeUpdate(connection));

        User user = new User();

        if(MutilConnectionPool.run("test", connection -> ModelSqlUtils.createSql(user).isTableExists(connection))){
            MutilConnectionPool.run("test", connection -> ModelSqlUtils.dropSql(user).dropTable(connection));
        }
        MutilConnectionPool.run("test", connection -> ModelSqlUtils.createSql(user).createTable(connection));

        user.setUserName("12345");
        MutilConnectionPool.run("test", connection -> ModelSqlUtils.insertSql(user).executeUpdate(connection));


        List<User> userList = MutilConnectionPool.run("test", connection -> ModelSqlUtils.selectSql(user).executeQuery(connection));
        System.out.println(userList);

        User updateUser = userList.get(0);
        updateUser.setUserName("54321");
        MutilConnectionPool.run("test", connection -> ModelSqlUtils.updateSql(updateUser).executeUpdate(connection));

        MutilConnectionPool.run("test", connection -> ModelSqlUtils.deleteSql(updateUser).executeUpdate(connection));
    }
}
