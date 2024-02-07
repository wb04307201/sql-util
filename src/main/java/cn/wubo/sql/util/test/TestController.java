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

        TypeReference typeReference1 = new TypeReference<Map<String, Object>>() {
        };
        System.out.println(typeReference1.clazz);

        TypeReference typeReference2 = new TypeReference<User>() {
        };
        System.out.println(typeReference2.clazz);

        TypeReference typeReference3 = new TypeReference<HashMap<String, Object>>() {
        };
        System.out.println(typeReference3.clazz);

        User user = new User();

        SQL sql = new SQL<User>(){};

        Boolean check = MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.isTableExists(connection));

        if (Boolean.TRUE.equals(check)) {
            MutilConnectionPool.run("test", connection -> new SQL<User>() {
            }.drop().parse().dropTable(connection));
        }

        MutilConnectionPool.run("test", connection -> new SQL<User>() {
        }.create().parse().createTable(connection));


    }
}
