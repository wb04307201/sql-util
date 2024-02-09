package cn.wubo.sql.util.test;

import cn.wubo.sql.util.ModelSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;

import java.util.List;

public class Test1Controller {

    /**
     * 主函数入口
     *
     * @param args 命令行参数
     */
    public static void main(String[] args) {
        // 判断数据源是否加载
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            // 加载数据源
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }

        User user = new User();
        // 判断表是否存在
        if (MutilConnectionPool.run("test", conn -> ModelSqlUtils.SQL(user).isTableExists(conn))) {
            // 删除表
            MutilConnectionPool.run("test", conn -> ModelSqlUtils.dropSql(user).dropTable(conn));
        }
        // 创建表
        MutilConnectionPool.run("test", conn -> ModelSqlUtils.createSql(user).createTable(conn));
        // 插入数据
        user.setUserName("112233");
        MutilConnectionPool.run("test", conn -> ModelSqlUtils.insertSql(user).executeUpdate(conn));
        // 查询数据
        List<User> userList = MutilConnectionPool.run("test", conn -> ModelSqlUtils.selectSql(user).executeQuery(conn));
        user.setId(userList.get(0).getId());
        user.setUserName("332211");
        // 更新数据
        MutilConnectionPool.run("test", conn -> ModelSqlUtils.updateSql(user).executeUpdate(conn));
        // 删除数据
        MutilConnectionPool.run("test", conn -> ModelSqlUtils.deleteSql(user).executeUpdate(conn));
    }



}
