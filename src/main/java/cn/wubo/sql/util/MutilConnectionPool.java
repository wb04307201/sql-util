package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ConnectionPoolException;
import com.alibaba.druid.pool.DruidDataSource;
import lombok.extern.slf4j.Slf4j;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
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
        DruidDataSource druidDataSource = new DruidDataSource();
        druidDataSource.setUrl(url);
        druidDataSource.setUsername(username);
        druidDataSource.setPassword(password);
        return getConnection(key, druidDataSource);
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
     * 根据给定的键移除连接池中的连接
     *
     * @param key 键值
     */
    public static synchronized void remove(String key) {
        if (poolMap.containsKey(key))
            poolMap.remove(key).close();
    }



    /**
     * 移除主数据源
     */
    public static synchronized void removeMaster() {
        remove(MASTER_DATASOURCE);
    }


    /**
     * 清空连接池中的所有连接
     */
    public static synchronized void clear() {
        for (Map.Entry<String, DruidDataSource> entry : poolMap.entrySet()) {
            entry.getValue().close();
        }
        poolMap.clear();
    }


    /**
     * 执行指定的BiFunction函数在一个新的数据库连接上
     *
     * @param key        数据库连接的key
     * @param url        数据库连接的URL
     * @param username   数据库连接的用户名
     * @param password   数据库连接的密码
     * @param biFunction 需要执行的BiFunction函数
     * @param u          BiFunction函数的参数
     * @param <U>        BiFunction函数的参数类型
     * @param <R>        BiFunction函数的返回值类型
     * @return BiFunction函数的返回值
     * @throws ConnectionPoolException 如果获取数据库连接失败
     */
    public static <U, R> R run(String key, String url, String username, String password, BiFunction<Connection, U, R> biFunction, U u) {
        try (Connection connection = getConnection(key, url, username, password)) {
            return biFunction.apply(connection, u);
        } catch (SQLException e) {
            throw new ConnectionPoolException(e);
        }
    }


    /**
     * 执行一个给定的函数作用于一个连接上，并返回结果。
     *
     * @param url        数据库连接URL
     * @param username   数据库用户名
     * @param password   数据库密码
     * @param biFunction 作用于连接上的函数
     * @param u          传入到函数中的参数
     * @param <U>        函数参数类型
     * @param <R>        函数返回值类型
     * @return 函数的返回值
     */
    public static <U, R> R run(String url, String username, String password, BiFunction<Connection, U, R> biFunction, U u) {
        // 调用run方法，使用MASTER_DATASOURCE主数据源执行函数作用于连接上，并返回结果
        return run(MASTER_DATASOURCE, url, username, password, biFunction, u);
    }


    /**
     * 执行指定的函数操作数据库，并返回结果。
     *
     * @param key        数据库连接池中的连接键
     * @param datasource 数据源
     * @param biFunction 数据库操作函数，接受一个连接和一个额外参数，并返回结果
     * @param u          额外参数
     * @param <U>        额外参数类型
     * @param <R>        返回结果类型
     * @return 操作结果
     * @throws ConnectionPoolException 连接池异常
     */
    public static <U, R> R run(String key, DataSource datasource, BiFunction<Connection, U, R> biFunction, U u) {
        try (Connection connection = getConnection(key, datasource)) {
            return biFunction.apply(connection, u);
        } catch (SQLException e) {
            throw new ConnectionPoolException(e);
        }
    }


    /**
     * 运行给定的函数并在指定的数据源上执行操作。
     *
     * @param datasource 数据源，指定要执行操作的数据源。
     * @param biFunction 运行的函数，接受一个连接和一个参数，并返回一个结果。
     * @param u          参数，传递给函数的参数。
     * @param <U>        参数类型。
     * @param <R>        结果类型。
     * @return 函数执行的结果。
     */
    public static <U, R> R run(DataSource datasource, BiFunction<Connection, U, R> biFunction, U u) {
        return run(MASTER_DATASOURCE, datasource, biFunction, u);
    }


}
