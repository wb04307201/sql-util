# sql-util 实体SQL工具类

[![](https://jitpack.io/v/com.gitee.wb04307201/sql-util.svg)](https://jitpack.io/#com.gitee.wb04307201/sql-util)

> 从实体类生成SQL增删改查语句  
> 包含创建表和删除表SQL  
> 另附带一个简单的数据库连接池

| 工具类                 | 描述                                                                                  |
|---------------------|-------------------------------------------------------------------------------------|
| ModelSqlUtils       | 从实体类生成建表、删表、增删改查等sql工具                                                              |
| ExecuteSqlUtils     | sql语句执行工具                                                                           |
| ConnectionPool      | 一个简单的链接池,通过新建ConnectionParam对象能快速的的初始化一个h2数据库连接池,也可在新建ConnectionParam对象时传入其他数据库配置信息 |
| MutilConnectionPool | 一个多数据源连接池                                                                           |
| SQL                 | SQL构造工具                                                                             |

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
    public List<User> test1() throws SQLException {
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
        return list4;
    }

    /**
     * 事务一致性
     *
     * @return
     * @throws SQLException
     */
    @GetMapping(value = "/test2")
    public void test2() throws SQLException, java.sql.SQLException, InterruptedException {
        // 1. 获取连接
        Connection connection = connectionPool.getConnection();
        // 2. 禁用自动提交
        connection.setAutoCommit(false);
        // TODO 3. 业务处理，比如使用ModelSqlUtils工具类下方法执行sql
        // 4. 提交代码
        connection.commit();
        // 5. 开启自动提交
        connection.setAutoCommit(true);
        // 6. 释放连接回连接池
        connectionPool.returnConnection(connection);
    }

    /**
     * 销毁所有连接
     * @throws Exception
     */
    @Override
    public void destroy() throws Exception {
        connectionPool.destory();
    }
}
```
