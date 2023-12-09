# sql-util 实体SQL工具类

[![](https://jitpack.io/v/com.gitee.wb04307201/sql-util.svg)](https://jitpack.io/#com.gitee.wb04307201/sql-util)

> 从实体类生成SQL增删改查语句  
> 包含创建表和删除表SQL  
> 另附带一个简单的数据库连接池

## 代码示例
1. 使用[文档在线预览](https://gitee.com/wb04307201/file-preview-spring-boot-starter)、[多平台文件存储](https://gitee.com/wb04307201/file-storage-spring-boot-starter)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[文件预览Demo](https://gitee.com/wb04307201/file-preview-demo)
2. 使用[多平台文件存储](https://gitee.com/wb04307201/file-storage-spring-boot-starter)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[文件存储Demo](https://gitee.com/wb04307201/file-storage-demo)
3. 使用[动态调度](https://gitee.com/wb04307201/dynamic-schedule-spring-boot-starter)、[消息中间件](https://gitee.com/wb04307201/message-spring-boot-starter)、[动态编译加载执行工具](https://gitee.com/wb04307201/loader-util)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[动态编码动态任务调度Demo](https://gitee.com/wb04307201/dynamic-schedule-demo)

| 工具类                 | 描述                                                                                  |
|---------------------|-------------------------------------------------------------------------------------|
| ModelSqlUtils       | 从实体类生成建表、删表、增删改查等sql工具 类                                                            |
| ExecuteSqlUtils     | sql语句执行工具类                                                                          |
| ConnectionPool      | 一个简单的链接池,通过新建ConnectionParam对象能快速的的初始化一个h2数据库连接池,也可在新建ConnectionParam对象时传入其他数据库配置信息 |
| MutilConnectionPool | 一个多数据源连接池                                                                           |
| SQL                 | SQL构造工具类                                                                            |

```java
@Slf4j
@RestController
public class TestController implements DisposableBean {

    /**
     * 创建连接池
     */
    private static ConnectionPool connectionPool = new ConnectionPool(new ConnectionParam());

    /**
     * 增删改查
     */
    @GetMapping(value = "/test1")
    public List<User> test1() {
        //检测表是否存在，不存在创建表
        if (Boolean.FALSE.equals(connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.isTableExists(connection, sql);
        }, SQL.tableExists("userInfo", DbType.h2)))) {
            connectionPool.run((connection, sqlStr) -> {
                return ExecuteSqlUtils.executeUpdate(connection, sqlStr);
            }, ModelSqlUtils.createSql("userInfo", User.class));
        }

        User user1 = new User();
        String id1 = UUID.randomUUID().toString();
        user1.setId(id1);
        user1.setName("aaaa");
        user1.setCode("aaaa");
        User user2 = new User();
        String id2 = UUID.randomUUID().toString();
        user2.setId(id2);
        user2.setName("bbbb");
        user2.setCode("bbbb");
        //插入数据
        connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeUpdate(connection, sql);
        }, ModelSqlUtils.insertSql("userInfo", user1));
        connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeUpdate(connection, sql);
        }, ModelSqlUtils.insertSql("userInfo", user2));
        //删除数据
        connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeUpdate(connection, sql);
        }, ModelSqlUtils.deleteByIdSql("userInfo", user1));
        //更新数据
        user2.setPassword("bbbb");
        connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeUpdate(connection, sql);
        }, ModelSqlUtils.updateByIdSql("userInfo", user2));
        //查询
        List<User> list1 = connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeQuery(connection, sql);
        }, ModelSqlUtils.selectSql("userInfo", new User()));
        log.debug(list1.toString());
        //使用id查询
        User query = new User();
        query.setId(id2);
        List<User> list2 = connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeQuery(connection, sql);
        }, ModelSqlUtils.selectSql("userInfo", query));
        log.debug(list2.toString());
        //查询返回Map
        SQL<Map> sqlMap = SQL.select(Map.class).table("userInfo").parse();
        List<Map> list3 = connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeQuery(connection, sql);
        }, sqlMap);
        log.debug(list3.toString());
        //分页查询
        List<User> list4 = connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeQuery(connection, sql);
        }, ModelSqlUtils.selectSql("userInfo", new User()).setDbtype(DbType.h2).page(1, 2));
        log.debug(list4.toString());

        // 泛型
        SQL<Map<String,Object>> mapSQL = SQL.select(new TypeReference<Map<String,Object>>(){},"*").table("userInfo").parse();
        List<Map<String,Object>> list5 = connectionPool.run((connection, sql) -> {
            return ExecuteSqlUtils.executeQuery(connection, sql);
        },mapSQL);
        log.debug(list5.toString());
        
        return list4;
    }

    /**
     * 事务一致性
     *
     * @return
     * @throws SQLException
     */
    @GetMapping(value = "/test2")
    public void test2() throws SQLException, InterruptedException {
        // 1. 获取连接
        Connection connection = connectionPool.getConnection();
        // 2. 禁用自动提交
        connection.setAutoCommit(false);
        // TODO 3. 业务处理，比如使用ExecuteSqlUtils工具类下方法执行sql
        // 4. 提交代码
        connection.commit();
        // 5. 开启自动提交
        connection.setAutoCommit(true);
        // 6. 释放连接回连接池
        connectionPool.returnConnection(connection);
    }

    /**
     * 销毁所有连接
     *
     * @throws Exception
     */
    @Override
    public void destroy() throws Exception {
        connectionPool.destory();
    }

    @GetMapping(value = "/test3")
    public List<Map<String, Object>> select() {
        String key = "";
        String url = "";
        String username = "";
        String passowrd = "";

        // 获取链接，也可以注入DataSource后通过DataSource初始化，仅支持DruidDataSource，重构方法没有key如参数时默认数据源key为master
        // 如果使用baomidou的多数据源，需要DynamicRoutingDataSource中获取真实的DataSource
        // 例如：ItemDataSource itemDataSource = (ItemDataSource) dataSource.getDataSource("master")
        //      itemDataSource.getRealDataSource()
        try (Connection connection = MutilConnectionPool.getConnection(key, url, username, passowrd)) {
            // 获取数据源后通过ExecuteSqlUtils工具类执行sql语句进行查询
            List<Map<String, Object>> list1 = ExecuteSqlUtils.executeQuery(connection, "select a.* from aaaa a", new HashMap<>(), new cn.wubo.sql.util.TypeReference<Map<String, Object>>() {
            });
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        // MutilConnectionPool工具类中的run方法，将ExecuteSqlUtils.executeQuery使用lambda表达式传入直接执行
        List<Map<String, Object>> list2 = MutilConnectionPool.run(key, url, username, passowrd, (conn, sql) -> ExecuteSqlUtils.executeQuery(conn, sql, new HashMap<>(), new cn.wubo.sql.util.TypeReference<Map<String, Object>>() {
        }), "select a.* from aaaa a");

        return list2;
    }
}
```
