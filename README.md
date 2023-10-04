# sql-util 实体SQL工具类

[![](https://jitpack.io/v/com.gitee.wb04307201/sql-util.svg)](https://jitpack.io/#com.gitee.wb04307201/sql-util)

> 从实体类生成SQL增删改查语句  
> 包含创建表和删除表SQL  
> 另附带一个简单的数据库连接池

| 工具类             | 描述                                                                                  |
|-----------------|-------------------------------------------------------------------------------------|
| ModelSqlUtils   | 从实体类生成建表、删表、增删改查等sql工具                                                              |
| ExecuteSqlUtils | sql语句执行工具                                                                           |
| ConnectionPool  | 一个简单的链接池,通过新建ConnectionParam对象能快速的的初始化一个h2数据库连接池,也可在新建ConnectionParam对象时传入其他数据库配置信息 |

```java

@Slf4j
@RestController
public class DemoController {

    //创建连接池
    private static ConnectionPool connectionPool = new ConnectionPool(new ConnectionParam());

    @GetMapping(value = "/test")
    public List<User> test() throws SQLException, InterruptedException {
        //从连接池获取连接
        Connection conn = connectionPool.getConnection();
        //检测表是否存在，不存在创建表
        if (Boolean.FALSE.equals(ExecuteSqlUtils.isTableExists(conn, "userInfo", DbType.h2))) {
            ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.createSql("userInfo", User.class));
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
        ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.insertSql("userInfo", user1));
        ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.insertSql("userInfo", user2));
        //删除数据
        ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.deleteByIdSql("userInfo", user1));
        //更新数据
        user2.setPassword("bbbb");
        ExecuteSqlUtils.executeUpdate(conn, ModelSqlUtils.updateByIdSql("userInfo", user2));
        //查询
        List<User> list = ExecuteSqlUtils.executeQuery(conn, ModelSqlUtils.selectSql("userInfo", new User()), User.class);
        log.debug(list.toString());
        //使用id查询
        User query = new User();
        query.setId(id2);
        list = ExecuteSqlUtils.executeQuery(conn, ModelSqlUtils.selectSql("userInfo", query), User.class);
        log.debug(list.toString());
        //分页查询
        list = ExecuteSqlUtils.executeQueryByPage(conn, ModelSqlUtils.selectSql("userInfo", new User()), DbType.h2, 1, 2, User.class);
        log.debug(list.toString());
        //释放连接
        connectionPool.returnConnection(conn);
        return list;
    }
}
```
