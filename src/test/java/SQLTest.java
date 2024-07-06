import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.web.EntityWebConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

@SpringBootTest(classes = EntityWebConfig.class)
class SQLTest {

    @Test
    void test() {
        // 判断数据源是否加载
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            // 加载数据源
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }
        SQL<User> userSQL = new SQL<User>() {
        };
        // 判断表是否存在
        if (Boolean.TRUE.equals(MutilConnectionPool.run("test", conn -> userSQL.isTableExists(conn)))) {
            // 删除表
            MutilConnectionPool.run("test", conn -> userSQL.drop().dropTable(conn));
        }
        // 创建表
        MutilConnectionPool.run("test", conn -> userSQL.create().createTable(conn));
        Assertions.assertEquals(true, MutilConnectionPool.check("test"));
        // 插入数据
        int count = MutilConnectionPool.run("test", conn -> userSQL.insert().addSet("user_name", "11111").executeUpdate(conn));
        Assertions.assertEquals(count, 1);
        // 更新数据
        count = MutilConnectionPool.run("test", conn -> userSQL.update().addSet("user_name", "22222").addWhereEQ("user_name", "11111").executeUpdate(conn));
        Assertions.assertEquals(count, 1);
        // 查询数据
        List<User> userList = MutilConnectionPool.run("test", conn -> userSQL.select().addWhereEQ("user_name", "22222").executeQuery(conn));
        Assertions.assertEquals(userList.size(), 1);
        // 删除数据
        count = MutilConnectionPool.run("test", conn -> userSQL.delete().addWhereEQ("user_name", "22222").executeUpdate(conn));
        Assertions.assertEquals(count, 1);
    }

}