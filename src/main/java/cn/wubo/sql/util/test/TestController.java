package cn.wubo.sql.util.test;

import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.TypeReference;

import java.util.HashMap;
import java.util.Map;

public class TestController {

    public static void main(String[] args) {
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }

        User user = new User();

        Boolean check = MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.isTableExists(connection));

        if (Boolean.TRUE.equals(check)) {
            MutilConnectionPool.run("test", connection -> new SQL<User>() {
            }.drop().parse().dropTable(connection));
        }

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.create().parse().createTable(connection));

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.insert().addSet("user_name","aaaa").parse().executeUpdate(connection));

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.delete().addWhereEQ("user_name","aaaa").parse().executeUpdate(connection));


    }
}
