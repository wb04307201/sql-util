package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ConnectionPoolException;
import com.alibaba.druid.pool.DruidDataSource;
import lombok.extern.slf4j.Slf4j;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;

/**
 * 多数据源连接池
 */
@Slf4j
public class MutilConnectionPool {

    private static final String MASTER_DATASOURCE = "master";

    private MutilConnectionPool() {
    }

    private static ConcurrentMap<String, DruidDataSource> poolMap = new ConcurrentHashMap<>();

    /**
     * 获取连接
     *
     * @param key      连接池的键
     * @param url      连接URL
     * @param username 用户名
     * @param password 密码
     * @return 数据库连接
     */
    public static synchronized Connection getConnection(String key, String url, String username, String password) {
        if (!poolMap.containsKey(key)) {
            DruidDataSource druidDataSource = new DruidDataSource();
            druidDataSource.setUrl(url);
            druidDataSource.setUsername(username);
            druidDataSource.setPassword(password);
            poolMap.putIfAbsent(key, druidDataSource);
        }
        try {
            return poolMap.get(key).getConnection();
        } catch (SQLException e) {
            throw new ConnectionPoolException(e);
        }
    }


    /**
     * 获取连接
     *
     * @param url      数据库连接URL
     * @param username 用户名
     * @param password 密码
     * @return 返回数据库连接
     */
    public static synchronized Connection getConnection(String url, String username, String password) {
        return getConnection(MASTER_DATASOURCE, url, username, password);
    }

    /**
     * 获取连接
     *
     * @param datasource 数据源
     * @return 连接
     */
    public static synchronized Connection getConnection(String key, DataSource datasource) {
        if (!(datasource instanceof DruidDataSource)) throw new ConnectionPoolException("请使用DruidDataSource!");
        if (!poolMap.containsKey(key)) poolMap.putIfAbsent(key, (DruidDataSource) datasource);
        try {
            return poolMap.get(key).getConnection();
        } catch (SQLException e) {
            throw new ConnectionPoolException(e);
        }
    }


    /**
     * 获取连接
     *
     * @param datasource 数据源
     * @return 连接
     */
    public static synchronized Connection getConnection(DataSource datasource) {
        return getConnection(MASTER_DATASOURCE, datasource);
    }


    /**
     * run方法
     * 对创建、获取、释放链接进一步进行封装
     * 允许通过biFunction参数传递方法进来执行
     * BiFunction的第一个参数必须为Connection
     */
    public static <U, R> R run(String key, String url, String username, String password, BiFunction<Connection, U, R> biFunction, U u) {
        try (Connection connection = getConnection(key, url, username, password)) {
            return biFunction.apply(connection, u);
        } catch (SQLException e) {
            throw new ConnectionPoolException(e);
        }
    }

}
