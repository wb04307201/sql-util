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

import static com.alibaba.druid.pool.DruidAbstractDataSource.*;

/**
 * 多数据源连接池
 */
@Slf4j
public class MutilConnectionPool {

    private static String masterDatasourceKey = "master";
    private static Integer initialSize = DEFAULT_INITIAL_SIZE;
    private static Integer maxActive = DEFAULT_MAX_ACTIVE_SIZE;
    private static Integer minIdle = DEFAULT_MIN_IDLE;
    private static Integer maxWait = DEFAULT_MAX_WAIT;
    private static Integer connectionErrorRetryAttempts = 1;
    private static Boolean breakAfterAcquireFailure = Boolean.FALSE;

    /**
     * 设置默认的主数据源键
     *
     * @param masterDatasourceKey 主数据源键
     */
    public static void setDefaultMasterDatasourceKey(String masterDatasourceKey) {
        MutilConnectionPool.masterDatasourceKey = masterDatasourceKey;
    }


    /**
     * 设置默认的初始大小
     *
     * @param initialSize 默认的初始大小
     */
    public static void setDefaultInitialSize(Integer initialSize) {
        MutilConnectionPool.initialSize = initialSize;
    }


    /**
     * 设置默认的最大连接数
     *
     * @param maxActive 最大连接数
     */
    public static void setDefaultMaxActive(Integer maxActive) {
        MutilConnectionPool.maxActive = maxActive;
    }


    /**
     * 设置默认的最小空闲连接数
     *
     * @param minIdle 最小空闲连接数
     */
    public static void setDefaultMinIdle(Integer minIdle) {
        MutilConnectionPool.minIdle = minIdle;
    }


    /**
     * 设置默认的最大等待时间
     *
     * @param maxWait 最大等待时间，以秒为单位
     */
    public static void setDefaultMaxWait(Integer maxWait) {
        MutilConnectionPool.maxWait = maxWait;
    }


    /**
     * 设置默认的连接错误重试次数
     *
     * @param connectionErrorRetryAttempts 连接错误重试次数
     */
    public static void setDefaultConnectionErrorRetryAttempts(Integer connectionErrorRetryAttempts) {
        MutilConnectionPool.connectionErrorRetryAttempts = connectionErrorRetryAttempts;
    }


    /**
     * 设置获取连接失败后是否中断连接池的使用
     *
     * @param breakAfterAcquireFailure 获取连接失败后是否中断连接池的使用
     */
    public static void setDefaultBreakAfterAcquireFailure(Boolean breakAfterAcquireFailure) {
        MutilConnectionPool.breakAfterAcquireFailure = breakAfterAcquireFailure;
    }



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
        druidDataSource.setUrl(url); // 设置数据库URL
        druidDataSource.setUsername(username); // 设置用户名
        druidDataSource.setPassword(password); // 设置密码
        druidDataSource.setInitialSize(initialSize);
        druidDataSource.setMaxActive(maxActive);
        druidDataSource.setMinIdle(minIdle);
        druidDataSource.setMaxWait(maxWait);
        druidDataSource.setConnectionErrorRetryAttempts(connectionErrorRetryAttempts); // 设置连接错误重试次数
        druidDataSource.setBreakAfterAcquireFailure(breakAfterAcquireFailure);
        return getConnection(key, druidDataSource); // 返回数据库连接
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
        return getConnection(masterDatasourceKey, url, username, password);
    }

    /**
     * 获取连接
     *
     * @param datasource 数据源
     * @return 连接
     */
    public static synchronized Connection getConnection(String key, DataSource datasource) {
        // 判断数据源是否为DruidDataSource，如果不是则抛出异常
        if (!(datasource instanceof DruidDataSource)) throw new ConnectionPoolException("请使用DruidDataSource!");
        // 如果poolMap中不包含指定key，则将数据源添加到poolMap中
        if (!poolMap.containsKey(key)) poolMap.putIfAbsent(key, (DruidDataSource) datasource);
        try {
            // 获取连接
            return poolMap.get(key).getConnection();
        } catch (SQLException e) {
            // 抛出ConnectionPoolException异常
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
        return getConnection(masterDatasourceKey, datasource);
    }

    /**
     * 根据给定的键移除连接池中的连接
     *
     * @param key 键值
     */
    public static synchronized void remove(String key) {
        if (poolMap.containsKey(key)) poolMap.remove(key).close();
    }


    /**
     * 移除主数据源
     */
    public static synchronized void removeMaster() {
        remove(masterDatasourceKey);
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
        // 调用run方法，使用masterDatasourceKey主数据源执行函数作用于连接上，并返回结果
        return run(masterDatasourceKey, url, username, password, biFunction, u);
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
        return run(masterDatasourceKey, datasource, biFunction, u);
    }
}
