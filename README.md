# sql-util 实体SQL工具类

[![](https://jitpack.io/v/com.gitee.wb04307201/sql-util.svg)](https://jitpack.io/#com.gitee.wb04307201/sql-util)

> 从实体类生成SQL增删改查语句  
> 包含创建表和删除表SQL  
> 另附带一个简单的数据库连接池

## 代码示例
1. 使用[文档在线预览](https://gitee.com/wb04307201/file-preview-spring-boot-starter)、[多平台文件存储](https://gitee.com/wb04307201/file-storage-spring-boot-starter)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[文件预览Demo](https://gitee.com/wb04307201/file-preview-demo)
2. 使用[多平台文件存储](https://gitee.com/wb04307201/file-storage-spring-boot-starter)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[文件存储Demo](https://gitee.com/wb04307201/file-storage-demo)
3. 使用[消息中间件](https://gitee.com/wb04307201/message-spring-boot-starter)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[消息发送代码示例](https://gitee.com/wb04307201/message-demo)
4. 使用[动态调度](https://gitee.com/wb04307201/dynamic-schedule-spring-boot-starter)、[消息中间件](https://gitee.com/wb04307201/message-spring-boot-starter)、[动态编译加载执行工具](https://gitee.com/wb04307201/loader-util)、[实体SQL工具类](https://gitee.com/wb04307201/sql-util)实现的[在线编码、动态调度、发送钉钉群消息Demo](https://gitee.com/wb04307201/dynamic-schedule-demo)

| 序号 | 工具类                 | 描述             |
|----|---------------------|----------------|
| 1  | MutilConnectionPool | 一个多数据源连接池      |
| 2  | ExecuteSqlUtils     | sql语句执行工具类     |
| 3  | SQL                 | SQL构造工具，执行工具   |
| 4  | ModelSqlUtils       | 从实体类构造SQL，执行工具 |

## 第一步 增加 JitPack 仓库
```xml
<repositories>
    <repository>
        <id>jitpack.io</id>
        <url>https://jitpack.io</url>
    </repository>
</repositories>
```

## 第二步 引入jar
```xml
<dependency>
    <groupId>com.gitee.wb04307201</groupId>
    <artifactId>sql-util</artifactId>
    <version>1.2.17</version>
</dependency>
```

#### MutilConnectionPool使用示例
```java

```

#### ModelSqlUtils、ExecuteSqlUtils、ConnectionPool、SQL的使用示例
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
#### MutilConnectionPool多数据源连接池的使用示例
```java
        // 获取链接，注意多数据源连接池仅支持传入DruidDatasource，如果需要使用多数据源，需要从DynamicRoutingDataSource中获取真实的DataSource，默认数据源key为master
        ItemDataSource itemDataSource = (ItemDataSource) dataSource.getDataSource("master");
        Connection connection1 = cn.wubo.sql.util.MutilConnectionPool.getConnection(itemDataSource.getRealDataSource());
        // 也可以使用自定义的数据源key用来区分连接池
        Connection connection2 = MutilConnectionPool.getConnection("datasourceKey", new com.alibaba.druid.pool.DruidDataSource());
        // 也可以通过数据源信息创建连接
        DataSourceInfo dataSourceInfo = new DataSourceInfo();
        Connection connection3 = MutilConnectionPool.getConnection(String.valueOf(dataSourceInfo.getDataSourceId()), dataSourceInfo.getUrl(), dataSourceInfo.getUserName(), dataSourceInfo.getPassword());

        // MutilConnectionPool工具类中的run方法，将ExecuteSqlUtils.executeQuery使用lambda表达式传入直接执行
        List<Map<String, Object>> list2 = MutilConnectionPool.run(itemDataSource.getRealDataSource(), (conn, sql) -> ExecuteSqlUtils.executeQuery(conn, sql, new HashMap<>(), new cn.wubo.sql.util.TypeReference<Map<String, Object>>() {
        }), "select a.* from aaaa a");
        List<Map<String, Object>> list3 = MutilConnectionPool.run(String.valueOf(dataSourceInfo.getDataSourceId()), dataSourceInfo.getUrl(), dataSourceInfo.getUserName(), dataSourceInfo.getPassword(), (conn, sql) -> ExecuteSqlUtils.executeQuery(conn, sql, new HashMap<>(), new cn.wubo.sql.util.TypeReference<Map<String, Object>>() {
        }), "select a.* from aaaa a");
        
        // 可设置连接池的默认配置，如连接池初始化连接数、最大连接数、最小连接数、最大等待时间等
        // 通过传入DruidDatasource的方式不支持上述配置
        MutilConnectionPool.setDefaultInitialSize
        MutilConnectionPool.setDefaultMaxActive
        MutilConnectionPool.setDefaultMinIdle
        MutilConnectionPool.setDefaultMaxWait
```
