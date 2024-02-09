package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ConnectionPoolException;
import cn.wubo.sql.util.utils.StringUtils;
import com.alibaba.druid.pool.DruidDataSource;
import lombok.extern.slf4j.Slf4j;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.alibaba.druid.pool.DruidAbstractDataSource.*;

/**
 * 多数据源连接池
 */
@Slf4j
public class MutilConnectionPool {
    private static Integer initialSize = DEFAULT_INITIAL_SIZE;
    private static Integer maxActive = DEFAULT_MAX_ACTIVE_SIZE;
    private static Integer minIdle = DEFAULT_MIN_IDLE;
    private static Integer maxWait = DEFAULT_MAX_WAIT;
    private static Integer connectionErrorRetryAttempts = 1;
    private static Boolean breakAfterAcquireFailure = Boolean.FALSE;

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
     * 检查key是否存在于缓存池中
     *
     * @param key 缓存池的key
     * @return true表示存在，false表示不存在
     */
    public static synchronized Boolean check(String key) {
        // 检查key是否为空
        StringUtils.isEmpty("key", key);
        // 检查key是否存在于缓存池中
        return poolMap.containsKey(key);
    }

    /**
     * 初始化数据源
     *
     * @param key      数据源的唯一标识
     * @param url      数据库URL
     * @param username 数据库用户名
     * @param password 数据库密码
     */
    public static synchronized void init(String key, String url, String username, String password) {
        // 参数有效性检查
        StringUtils.isEmpty("key", key);
        // 参数有效性检查
        StringUtils.isEmpty("url, username", url, username);
        // 创建DruidDataSource对象
        DruidDataSource druidDataSource = new DruidDataSource();
        druidDataSource.setUrl(url); // 设置数据库URL
        druidDataSource.setUsername(username); // 设置用户名
        druidDataSource.setPassword(Objects.requireNonNull(password, "参数password不能为空！")); // 设置密码
        druidDataSource.setInitialSize(initialSize);
        druidDataSource.setMaxActive(maxActive);
        druidDataSource.setMinIdle(minIdle);
        druidDataSource.setMaxWait(maxWait);
        druidDataSource.setConnectionErrorRetryAttempts(connectionErrorRetryAttempts); // 设置连接错误重试次数
        druidDataSource.setBreakAfterAcquireFailure(breakAfterAcquireFailure);
        // 将DruidDataSource对象存入map中
        poolMap.putIfAbsent(key, druidDataSource);
    }

    /**
     * 初始化连接池
     *
     * @param key        连接池的key
     * @param datasource 数据源
     */
    public static synchronized void init(String key, DataSource datasource) {
        StringUtils.isEmpty("key", key);
        if (!(datasource instanceof DruidDataSource)) throw new ConnectionPoolException("请使用DruidDataSource!");
        // 将数据源存入map中
        poolMap.putIfAbsent(key, (DruidDataSource) datasource);
    }

    /**
     * 获取连接
     *
     * @param key 连接池的键
     * @return 数据库连接
     */
    public static synchronized Connection getConnection(String key) {
        // 检查参数是否为空
        StringUtils.isEmpty("key", key);

        // 检查连接池是否已初始化
        if (poolMap.containsKey(key)) {
            try {
                // 获取连接
                return poolMap.get(key).getConnection();
            } catch (SQLException e) {
                // 抛出异常
                throw new ConnectionPoolException(e.getMessage(), e);
            }
        } else {
            // 抛出异常
            throw new ConnectionPoolException("数据源还未初始化");
        }
    }

    /**
     * 根据给定的键移除连接池中的连接
     *
     * @param key 键值
     */
    public static synchronized void remove(String key) {
        // 检查连接池中是否存在该键
        if (poolMap.containsKey(key)) {
            // 关闭连接池
            poolMap.get(key).close();
            // 移除连接池
            poolMap.remove(key);
        }
    }

    /**
     * 清空连接池中的所有连接
     */
    public static synchronized void clear() {
        // 遍历连接池中的所有键值对
        for (Map.Entry<String, DruidDataSource> entry : poolMap.entrySet()) {
            // 关闭连接池
            entry.getValue().close();
        }
        // 清空连接池
        poolMap.clear();
    }

    /**
     * 在数据库连接上执行Function函数
     *
     * @param key      数据库连接的key
     * @param function 数据库连接和返回值类型的Function函数
     * @param <R>      返回值类型
     * @return 执行结果
     * @throws ConnectionPoolException 获取数据库连接失败时抛出的异常
     */
    public static <R> R run(String key, Function<Connection, R> function) {
        try (Connection connection = getConnection(key)) {
            // 在数据库连接上执行Function函数
            return function.apply(connection);
        } catch (SQLException e) {
            // 如果获取数据库连接失败，则抛出ConnectionPoolException异常
            throw new ConnectionPoolException(e);
        }
    }
}
