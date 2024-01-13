package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.DataTableEntity;
import cn.wubo.sql.util.entity.MethodEntity;
import cn.wubo.sql.util.exception.ExecuteSqlUtilsException;
import com.alibaba.druid.DbType;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.sql.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

@Slf4j
public class ExecuteSqlUtils {

    private static final String MAP_NAME = "java.util.Map";

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
            throw new ExecuteSqlUtilsException(e);
        }
    }


    /**
     * 执行查询操作并返回结果集
     *
     * @param connection 数据库连接对象
     * @param sql        查询语句
     * @param clazz      查询结果集的类型
     * @param <T>        查询结果集的类型
     * @return 查询结果集的列表
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Class<T> clazz) {
        return executeQuery(connection, sql, new HashMap<>(), clazz);
    }


    /**
     * 执行查询操作并返回结果列表
     *
     * @param connection 数据库连接对象
     * @param sql        SQL语句对象
     * @param <T>        结果类型
     * @return 查询结果列表
     */
    public static <T> List<T> executeQuery(Connection connection, SQL<T> sql) {
        return executeQuery(connection, sql.getParse(), sql.getParams(), sql.getClazz());
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
        return executeQuery(connection, sql, params, (Class<T>) ((ParameterizedType) typeReference.type).getRawType());
    }


    /**
     * 执行查询操作并返回结果列表
     *
     * @param connection 数据库连接对象
     * @param sql        SQL查询语句
     * @param type       结果类型的引用
     * @param <T>        结果类型的参数
     * @return 结果列表
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, TypeReference<T> type) {
        return executeQuery(connection, sql, new HashMap<>(), (Class<T>) ((ParameterizedType) type.type).getRawType());
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
            throw new ExecuteSqlUtilsException(e);
        }
    }


    /**
     * 执行更新操作
     *
     * @param connection 数据库连接对象
     * @param sql        SQL对象
     * @param <T>        SQL泛型类型
     * @return 更新的行数
     */
    public static <T> int executeUpdate(Connection connection, SQL<T> sql) {
        return executeUpdate(connection, sql.getParse(), sql.getParams());
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
        if (Boolean.TRUE.equals(isMap(clazz))) return result2Map(rs, clazz);
        else return result2Class(rs, clazz);
    }


    /**
     * 将ResultSet转换为List集合
     *
     * @param rs    ResultSet对象
     * @param clazz 类型参数
     * @return 转换后的List集合
     * @throws SQLException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws NoSuchMethodException
     * @throws InvocationTargetException
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
            T row = createMap(clazz);
            for (int i = 1; i <= count; i++)
                method.invoke(row, headers[i - 1], rs.getObject(i));
            result.add(row);
        }
        return result;
    }


    private static <T> Boolean isMap(Class<T> clazz) {
        // 判断给定的类是否是Map接口或其实现类
        return clazz.getName().equals(MAP_NAME) || Arrays.stream(clazz.getInterfaces()).anyMatch(item -> item.getName().equals(MAP_NAME)) || Arrays.stream(clazz.getSuperclass().getInterfaces()).anyMatch(item -> item.getName().equals(MAP_NAME));
    }


    /**
     * 根据给定的类类型创建并返回一个空的Map对象。
     *
     * @param clazz Map对象的类类型
     * @return 创建的空的Map对象
     * @throws InstantiationException 当实例化创建Map对象时发生异常
     * @throws IllegalAccessException 当访问实例化创建的Map对象时发生异常
     */
    private static <T> T createMap(Class<T> clazz) throws InstantiationException, IllegalAccessException {
        if (clazz == Properties.class) return (T) new Properties();
        if (clazz == Hashtable.class) return (T) new Hashtable<>();
        if (clazz == IdentityHashMap.class) return (T) new IdentityHashMap<>();
        if (clazz == SortedMap.class || clazz == TreeMap.class) return (T) new TreeMap<>();
        if (clazz == ConcurrentMap.class || clazz == ConcurrentHashMap.class) return (T) new ConcurrentHashMap<>();
        if (clazz == Map.class || clazz == HashMap.class) return (T) new HashMap<>();
        if (clazz == LinkedHashMap.class) return (T) new LinkedHashMap<>();
        return clazz.newInstance();
    }

    /**
     * 将ResultSet中的数据转换为指定类的列表
     *
     * @param rs    ResultSet对象，包含要转换的数据
     * @param clazz 要转换为的实体类的Class对象
     * @return 转换后的实体类列表
     * @throws SQLException           如果在执行SQL操作时发生错误
     * @throws InstantiationException 如果实例化实体类失败
     * @throws IllegalAccessException 如果无法访问实体类的的方法或属性
     */
    private static <T> List<T> result2Class(ResultSet rs, Class<T> clazz) throws SQLException, InstantiationException, IllegalAccessException {
        // 创建实体类列表
        List<T> result = new ArrayList<>();

        // 获取实体类中的方法信息
        HashMap<String, MethodEntity> hmMethods = new HashMap<>();
        Arrays.stream(clazz.getDeclaredMethods()).forEach(method -> {
            MethodEntity methodEntity = new MethodEntity();

            // 获取方法的名称
            String methodName = method.getName();
            String methodKey = methodName.toUpperCase();

            // 获取方法的参数
            Class<?>[] paramTypes = method.getParameterTypes();
            methodEntity.setMethodName(methodName);
            methodEntity.setMethodParamTypes(paramTypes);

            // 处理方法重载
            if (hmMethods.containsKey(methodKey)) {
                methodEntity.setRepeatMethodNum(methodEntity.getRepeatMethodNum() + 1);
                methodEntity.setRepeatMethodsParamTypes(paramTypes);
            } else {
                hmMethods.put(methodKey, methodEntity);
            }
        });

        // 获取 ResultSet 的元数据信息
        ResultSetMetaData rsMetaData = rs.getMetaData();
        int columnCount = rsMetaData.getColumnCount();

        // 创建 DataTableEntity 对象
        DataTableEntity dataTable = new DataTableEntity(columnCount);
        for (int i = 0; i < columnCount; i++) {
            String columnName = rsMetaData.getColumnName(i + 1);
            int columnType = rsMetaData.getColumnType(i + 1);

            // 获取字段名称和类型
            dataTable.setColumnName(columnName, i);
            dataTable.setColumnType(columnType, i);
        }

        // 处理 ResultSet 数据信息
        while (rs.next()) {
            // 转换为实体类并添加到列表中
            result.add(toRow(rs, clazz, dataTable, hmMethods));
        }

        // 返回实体类列表
        return result;
    }


    /**
     * 将ResultSet中的数据转换为指定类的实例对象
     *
     * @param rs        ResultSet对象，用于获取数据
     * @param clazz     转换后的对象的类型
     * @param dataTable DataTableEntity对象，用于获取列信息
     * @param hmMethods 用于存储方法信息的HashMap对象，键为方法名，值为MethodEntity对象
     * @return 转换后的对象
     * @throws InstantiationException 当实例化对象失败时抛出该异常
     * @throws IllegalAccessException 当访问受保护的字段时抛出该异常
     */
    private static <T> T toRow(ResultSet rs, Class<T> clazz, DataTableEntity dataTable, HashMap<String, MethodEntity> hmMethods) throws InstantiationException, IllegalAccessException {
        T row = clazz.newInstance();
        String[] strColumnNames = dataTable.getColumnNames();
        Arrays.stream(strColumnNames).forEach(columnName -> {
            try {
                // 获取字段值
                Object objColumnValue = rs.getObject(columnName);
                if (objColumnValue != null) {
                    // 获取set方法名
                    String strMethodKey = "SET" + columnName.toUpperCase();
                    // 值和方法都不为空,这里方法名不为空即可,值可以为空的
                    // 判断字段的类型,方法名，参数类型
                    MethodEntity methodEntity = hmMethods.get(strMethodKey);
                    if (methodEntity != null) {
                        String methodName = methodEntity.getMethodName();
                        int repeatMethodNum = methodEntity.getRepeatMethodNum();
                        Class<?>[] paramTypes = methodEntity.getMethodParamTypes();
                        Method method = clazz.getMethod(methodName, paramTypes);
                        // 如果重载方法数 > 1，则判断是否有java.lang.IllegalArgumentException异常，循环处理
                        trySetValue(clazz, method, row, objColumnValue, repeatMethodNum, methodEntity, methodName);
                    }
                }
            } catch (SQLException | NoSuchMethodException e) {
                log.debug(e.getMessage(), e);
            }
        });
        return row;
    }

    private static <T> void trySetValue(Class<T> clazz, Method method, T row, Object objColumnValue, int repeatMethodNum, MethodEntity methodEntity, String methodName) throws NoSuchMethodException {
        try {
            // 尝试设置参数值
            method.invoke(row, objColumnValue);
        } catch (IllegalArgumentException | InvocationTargetException | IllegalAccessException e) {
            log.debug(e.getMessage(), e);
            // 处理重载方法
            tryOverride(clazz, repeatMethodNum, methodEntity, methodName, row, objColumnValue);
        }
    }


    /**
     * 尝试覆盖指定类中的重载方法，将指定参数值传入方法并调用
     *
     * @param clazz           指定的类对象
     * @param repeatMethodNum 重载方法的个数
     * @param methodEntity    包含重载方法信息的实体对象
     * @param methodName      重载方法名
     * @param row             指定的对象行（用于确定调用的类）
     * @param objColumnValue  参数值对象
     * @throws NoSuchMethodException 如果找不到指定的重载方法
     */
    private static <T> void tryOverride(Class<T> clazz, int repeatMethodNum, MethodEntity methodEntity, String methodName, T row, Object objColumnValue) throws NoSuchMethodException {
        Method method;
        for (int j = 1; j < repeatMethodNum; j++) {
            try {
                Class<?>[] repeatParamTypes = methodEntity.getRepeatMethodsParamTypes(j - 1);
                method = clazz.getMethod(methodName, repeatParamTypes);
                method.invoke(row, objColumnValue);
                break;
            } catch (IllegalArgumentException | InvocationTargetException | IllegalAccessException ex) {
                log.debug(ex.getMessage(), ex);
            }
        }
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
     * 判断数据库中是否存在指定表。
     *
     * @param connection 数据库连接对象
     * @param sql        指定的SQL对象
     * @param <T>        泛型类型
     * @return 如果表存在则返回true，否则返回false
     */
    public static <T> Boolean isTableExists(Connection connection, SQL<T> sql) {
        String tableName = sql.getTable();
        DbType dbType = sql.getDbType();

        // 将表名转换为大写（H2、Oracle、PostgreSQL、DB2、DM数据库类型）
        if ((DbType.h2.equals(dbType) || DbType.oracle.equals(dbType) || DbType.postgresql.equals(dbType) || DbType.db2.equals(dbType) || DbType.dm.equals(dbType)))
            tableName = tableName.toUpperCase();

            // 将表名转换为小写（其他数据库类型）
        else tableName = tableName.toLowerCase();

        // 调用isTableExists方法判断表是否存在
        return isTableExists(connection, null, null, tableName, new String[]{"TABLE"});
    }


}
