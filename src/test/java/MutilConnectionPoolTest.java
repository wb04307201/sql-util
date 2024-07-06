import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.web.EntityWebConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.sql.Connection;
import java.sql.SQLException;

@SpringBootTest(classes = EntityWebConfig.class)
class MutilConnectionPoolTest {

    @Test
    void test() throws SQLException {
        // 判断数据源是否加载
        if (Boolean.FALSE.equals(MutilConnectionPool.check("test"))) {
            // 加载数据源
            MutilConnectionPool.init("test", "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", "sa", "");
        }
        // 获取数据源，注意使用完之后释放连接
        Connection connection = MutilConnectionPool.getConnection("test");
        connection.close();
        // 通过传入函数式接口执行方法，内部会自动创建连接并在使用之后释放
        MutilConnectionPool.run("test", conn -> null);
    }
}