package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.exception.ExecuteSqlException;
import cn.wubo.sql.util.utils.MapUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ExecuteSqlUtils {

    private ExecuteSqlUtils() {
    }

    /**
     * 执行查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @param <T>        泛型
     * @return List<T>
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Map<Integer, Object> params, Class<T> clazz) {
        log.debug("executeQuery ...... sql:{} params:{} class:{}", sql, params, clazz.getName());
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet())
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            return getResult(preparedStatement.executeQuery(), clazz);
        } catch (SQLException | NoSuchMethodException | InstantiationException | IllegalAccessException |
                 InvocationTargetException e) {
            throw new ExecuteSqlException(e);
        }
    }

    /**
     * 执行查询操作并返回结果列表
     *
     * @param connection    数据库连接对象
     * @param sql           SQL查询语句
     * @param params        参数映射
     * @param typeReference 结果类型引用
     * @param <T>           结果类型
     * @return 查询结果列表
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Map<Integer, Object> params, TypeReference<T> typeReference) {
        return executeQuery(connection, sql, params, typeReference.clazz);
    }

    /**
     * 执行查询操作，返回查询结果的列表
     *
     * @param connection    数据库连接对象
     * @param sql           SQL查询语句
     * @param typeReference 查询结果的类型引用
     * @return 查询结果的列表
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, TypeReference<T> typeReference) {
        return executeQuery(connection, sql, new HashMap<>(), typeReference.clazz);
    }

    /**
     * 执行更新数据库操作
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @return int
     */
    public static int executeUpdate(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeUpdate ...... sql:{} params:{}", sql, params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet())
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            return preparedStatement.executeUpdate();
        } catch (SQLException e) {
            throw new ExecuteSqlException(e);
        }
    }

    /**
     * 执行更新操作
     *
     * @param connection 数据库连接对象
     * @param sql        SQL语句
     * @return 更新的行数
     */
    public static int executeUpdate(Connection connection, String sql) {
        return executeUpdate(connection, sql, new HashMap<>());
    }

    /**
     * 执行多个SQL语句并返回受影响的行数
     * @param connection 数据库连接对象
     * @param sqls SQL语句列表
     * @return 受影响的行数
     */
    public static int executeUpdate(Connection connection, List<String> sqls) {
        int count = 0;
        try {
            connection.setAutoCommit(false); // 设置自动提交为false，禁止自动提交事务
            for (String sql : sqls) {
                count += executeUpdate(connection, sql); // 执行单个SQL语句并累加受影响的行数
            }
            connection.commit(); // 提交事务
            connection.setAutoCommit(true); // 设置自动提交为true，允许自动提交事务
        } catch (SQLException e) {
            throw new ExecuteSqlException(e); // 抛出执行SQL异常
        }
        return count; // 返回受影响的行数
    }

    /**
     * 处理返回值
     *
     * @param rs  游标
     * @param <T> 泛型
     * @return List<T> List<T>类型的返回结果
     * @throws NoSuchMethodException     当找不到clazz类的无参构造方法时抛出该异常
     * @throws InstantiationException    当使用无参构造方法实例化clazz类失败时抛出该异常
     * @throws IllegalAccessException    当无权访问clazz类的属性或方法时抛出该异常
     * @throws SQLException              当访问数据库发生异常时抛出该异常
     * @throws InvocationTargetException 当调用方法发生异常时抛出该异常
     */
    private static <T> List<T> getResult(ResultSet rs, Class<T> clazz) throws NoSuchMethodException, InstantiationException, IllegalAccessException, SQLException, InvocationTargetException {
        log.debug("getResult ...... class:{}", clazz.getName());
        if (Boolean.TRUE.equals(MapUtils.isMap(clazz))) return result2Map(rs, clazz);
        else return result2Class(rs, clazz);
    }

    /**
     * 将ResultSet转换为Map列表
     *
     * @param rs    ResultSet对象
     * @param clazz Map的泛型类型
     * @return 转换后的Map列表
     * @throws SQLException              如果出现SQL异常
     * @throws InstantiationException    如果出现实例化异常
     * @throws IllegalAccessException    如果出现访问权限异常
     * @throws NoSuchMethodException     如果出现方法不存在异常
     * @throws InvocationTargetException 如果出现方法调用目标异常
     */
    private static <T> List<T> result2Map(ResultSet rs, Class<T> clazz) throws SQLException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
        log.debug("result2Map ...... ");
        List<T> result = new ArrayList<>();
        ResultSetMetaData rsmd = rs.getMetaData();
        int count = rsmd.getColumnCount();// 获取列的数量
        // 列头
        String[] headers = new String[count];
        for (int i = 1; i <= count; i++)
            headers[i - 1] = rsmd.getColumnLabel(i);
        // 数据
        Method method = clazz.getMethod("put", Object.class, Object.class);
        while (rs.next()) {
            T row = MapUtils.createMap(clazz);
            for (int i = 1; i <= count; i++)
                method.invoke(row, headers[i - 1], rs.getObject(i));
            result.add(row);
        }
        return result;
    }

    /**
     * 将ResultSet转换为指定类型的List集合
     *
     * @param rs    ResultSet对象
     * @param clazz 指定的类型
     * @return 转换后的List集合
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SQLException
     */
    private static <T> List<T> result2Class(ResultSet rs, Class<T> clazz) throws InstantiationException, IllegalAccessException, SQLException {
        List<T> result = new ArrayList<>();
        TableModel tableModel = EntityUtils.getTable(clazz);
        ResultSetMetaData rsmd = rs.getMetaData();
        int count = rsmd.getColumnCount();
        Map<String, TableModel.ColumnModel> headerMap = new HashMap<>();
        for (int i = 1; i <= count; i++) {
            String header = rsmd.getColumnLabel(i);
            tableModel.getCols().stream().filter(col -> header.equalsIgnoreCase(col.getColumnName())).findAny().ifPresent(col -> headerMap.put(header, col));
        }
        while (rs.next()) {
            T row = clazz.newInstance();
            headerMap.entrySet().forEach(entry -> {
                Field field = entry.getValue().getField();
                field.setAccessible(true);
                try {
                    field.set(row, EntityUtils.getValue(entry.getValue(), rs.getObject(entry.getKey())));
                } catch (IllegalAccessException | SQLException e) {
                    throw new ExecuteSqlException(e);
                }
            });
            result.add(row);
        }
        return result;
    }

    /**
     * 判断表是否存在
     *
     * @param connection       数据库连接
     * @param catalog          数据库名
     * @param schemaPattern    tableNamePattern
     * @param tableNamePattern 表名
     * @param types            类型标准（数组格式），一般使用"TABLE"，即获取所有类型为TABLE的表
     * @return boolean
     */
    public static Boolean isTableExists(Connection connection, String catalog, String schemaPattern, String tableNamePattern, String[] types) {
        boolean isTableExists = false;
        try {
            ResultSet rset = connection.getMetaData().getTables(catalog, schemaPattern, tableNamePattern, types);
            if (rset.next()) isTableExists = true;
        } catch (SQLException e) {
            log.error(e.getMessage(), e);
        }
        return isTableExists;
    }

    /**
     * 判断表是否存在
     *
     * @param connection 数据库连接对象
     * @param tableName  表名
     * @return 表是否存在
     */
    public static Boolean isTableExists(Connection connection, String tableName) {
        return isTableExists(connection, null, null, tableName, new String[]{"TABLE"});
    }
}
