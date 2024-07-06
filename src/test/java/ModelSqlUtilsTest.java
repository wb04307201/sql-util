import cn.wubo.sql.util.ModelSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.web.EntityWebConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

@SpringBootTest(classes = EntityWebConfig.class)
class ModelSqlUtilsTest {

    @Test
    void test() {
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
        int count = MutilConnectionPool.run("test", conn -> ModelSqlUtils.saveSql(user).executeUpdate(conn));
        Assertions.assertEquals(count, 1);
        // 查询数据
        List<User> userList = MutilConnectionPool.run("test", conn -> ModelSqlUtils.selectSql(user).executeQuery(conn));
        Assertions.assertEquals(userList.size(), 1);
        user.setId(userList.get(0).getId());
        user.setUserName("332211");
        // 更新数据
        count = MutilConnectionPool.run("test", conn -> ModelSqlUtils.saveSql(user).executeUpdate(conn));
        Assertions.assertEquals(count, 1);
        // 可以使用 ModelSqlUtils.insertSql(user)强行插入数据
        // 可以使用 ModelSqlUtils.updateSql(user)强行更新数据
        // 删除数据
        count = MutilConnectionPool.run("test", conn -> ModelSqlUtils.deleteSql(user).executeUpdate(conn));
        Assertions.assertEquals(count, 1);
    }

}