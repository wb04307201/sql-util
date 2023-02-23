# sql-util 实体SQL工具类

[![](https://jitpack.io/v/com.gitee.wb04307201/sql-util.svg)](https://jitpack.io/#com.gitee.wb04307201/sql-util)

ConnectionPool 一个简单的链接池，能快速的的初始化一个h2数据库  
ExecuteSqlUtils sql语句执行工具  
ModelSqlUtils 从实体类生成建表、删表、增删改查等sql工具 

```java
@RestController
public class DemoController {

    //创建连接池
    private static ConnectionPool connectionPool = new ConnectionPool(new ConnectionParam());

    @GetMapping(value = "/test")
    public List<User> test() throws SQLException, InterruptedException {
        //获取连接池
        Connection conn = connectionPool.getConnection();
        //检测表是否存在，不存在创建表
        if (!ExecuteSqlUtils.isTableExists(conn, "userInfo", connectionPool.getDbType())) {
            ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.createSql("userInfo", new User()), new HashMap<>());
        }
        User user = new User();
        user.setUserCode("aaaa");
        user.setUserName("bbb");
        //插入数据
        ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.insertSql("userInfo", user), new HashMap<>());
        //查询
        List<User> list = ExecuteSqlUtils.executeQuery(conn, ModelSqlUtils.selectSql("userInfo", new User()), new HashMap<>(), User.class);
        connectionPool.returnConnection(conn);
        return list;
    }
}
```
