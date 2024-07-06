import cn.wubo.sql.util.ExecuteSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.TypeReference;
import cn.wubo.sql.util.web.EntityWebConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(classes = EntityWebConfig.class)
class ExecuteSqlUtilsDemoTest {

    @Test
    void test() throws SQLException {
        // 判断数据源是否加载
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            // 加载数据源
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }

        Connection connection = MutilConnectionPool.getConnection("test");

        // 判断表是否存在
        Boolean check = ExecuteSqlUtils.isTableExists(connection, "test_user".toUpperCase());
        // 通过MutilConnectionPool.run检查表是否存在
        check = MutilConnectionPool.run("test", conn -> ExecuteSqlUtils.isTableExists(conn, "test_user".toUpperCase()));

        Map<Integer, Object> params = new HashMap<>();
        params.put(1, "123123");
        // 执行插入、更新的sql语句
        int count = ExecuteSqlUtils.executeUpdate(connection, "update test_user set user_name = ?", params);
        count = MutilConnectionPool.run("test", conn -> ExecuteSqlUtils.executeUpdate(conn, "update test_user set user_name = ?", params));

        // 执行查询的sql语句
        List<Map<String, Object>> list = ExecuteSqlUtils.executeQuery(connection, "select * from test_user where user_name = ?", params, new TypeReference<Map<String, Object>>() {
        });
        list = MutilConnectionPool.run("test", conn -> ExecuteSqlUtils.executeQuery(conn, "select * from test_user where user_name = ?", params, new TypeReference<Map<String, Object>>() {
        }));

        // 执行删除的sql语句
        MutilConnectionPool.run("test", conn -> ExecuteSqlUtils.executeUpdate(conn, "delete from test_user where user_name = ?", params));

        connection.close();
    }

}