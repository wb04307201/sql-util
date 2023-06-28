package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.DataTableEntity;
import cn.wubo.sql.util.entity.MethodEntity;
import cn.wubo.sql.util.exception.SqlUtilException;
import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.PagerUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.*;
import java.util.*;

@Slf4j
public class ExecuteSqlUtils {

    private ExecuteSqlUtils() {
    }

    /**
     * 执行查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @return List<Map < String, Object>>
     */
    public static List<Map<String, Object>> executeQuery(Connection connection, String sql) {
        return executeQuery(connection, sql, new HashMap<>());
    }

    /**
     * 执行查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @return List<Map < String, Object>>
     */
    public static List<Map<String, Object>> executeQuery(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeQuery ...... sql:{} params:{}", sql, params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return getResultMap(preparedStatement.executeQuery());
        } catch (SQLException e) {
            throw new SqlUtilException(e);
        }
    }

    /**
     * 执行查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param clasz      类
     * @param <T>        泛型
     * @return List<T>
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Class<T> clasz) {
        return executeQuery(connection, sql, new HashMap<>(), clasz);
    }

    /**
     * 执行查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @param clasz      类
     * @param <T>        泛型
     * @return List<T>
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Map<Integer, Object> params, Class<T> clasz) {
        log.debug("executeQuery ...... sql:{} params:{}", sql, params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return getResultMap(preparedStatement.executeQuery(), clasz);
        } catch (SQLException | NoSuchMethodException | InstantiationException | IllegalAccessException e) {
            throw new SqlUtilException(e);
        }
    }

    /**
     * 执行分页查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @param dbType     数据库类型
     * @param offset     偏移量
     * @param count      数量
     * @return List<Map < String, Object>>
     */
    public static List<Map<String, Object>> executeQueryByPage(Connection connection, String sql, Map<Integer, Object> params, String dbType, int offset, int count) {
        return executeQuery(connection, PagerUtils.limit(sql, DbType.valueOf(dbType), offset, count), params);
    }

    /**
     * 执行分页查询
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @param dbType     数据库类型
     * @param offset     偏移量
     * @param count      数量
     * @param clasz      类
     * @param <T>        泛型
     * @return List<T>
     */
    public static <T> List<T> executeQueryByPage(Connection connection, String sql, Map<Integer, Object> params, String dbType, int offset, int count, Class<T> clasz) {
        return executeQuery(connection, PagerUtils.limit(sql, DbType.valueOf(dbType), offset, count), params, clasz);
    }

    /**
     * 执行数据库操作
     *
     * @param connection 数据库连接
     * @param sql        sql
     * @param params     参数
     * @return int
     */
    public static int executeUpdate(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeUpdate ...... sql:{} params:{}", sql, params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return preparedStatement.executeUpdate();
        } catch (SQLException e) {
            throw new SqlUtilException(e);
        }
    }


    /**
     * 处理返回值
     *
     * @param rs
     * @return
     * @throws SQLException
     */
    public static List<Map<String, Object>> getResultMap(ResultSet rs) throws SQLException {
        log.debug("getResultMap ...... ");
        List<Map<String, Object>> result = new ArrayList<>();
        ResultSetMetaData rsmd = rs.getMetaData();
        int count = rsmd.getColumnCount();// 获取列的数量
        //列头
        String[] headers = new String[count];
        for (int i = 1; i <= count; i++)
            headers[i - 1] = rsmd.getColumnLabel(i);
        //数据
        while (rs.next()) {
            Map<String, Object> row = new HashMap<>();
            for (int i = 1; i <= count; i++)
                row.put(headers[i - 1], rs.getObject(i));
            result.add(row);
        }
        return result;
    }

    /**
     * 处理返回值
     *
     * @param rs    游标
     * @param clazz 类
     * @param <T>   泛型
     * @return List<T>
     * @throws NoSuchMethodException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SQLException
     */
    public static <T> List<T> getResultMap(ResultSet rs, Class<T> clazz) throws NoSuchMethodException, InstantiationException, IllegalAccessException, SQLException {
        log.debug("getResultMap ...... class:{}", clazz.getName());
        List<T> result = new ArrayList<>();
        //获取实体中定义的方法
        HashMap<String, MethodEntity> hmMethods = new HashMap<>();
        Arrays.stream(clazz.getDeclaredMethods()).forEach(method -> {
            MethodEntity methodEntity = new MethodEntity();
            //方法的名称
            String methodName = method.getName();
            String methodKey = methodName.toUpperCase();
            //方法的参数
            Class[] paramTypes = method.getParameterTypes();
            methodEntity.setMethodName(methodName);
            methodEntity.setMethodParamTypes(paramTypes);
            //处理方法重载
            if (hmMethods.containsKey(methodKey)) {
                methodEntity.setRepeatMethodNum(methodEntity.getRepeatMethodNum() + 1);
                methodEntity.setRepeatMethodsParamTypes(paramTypes);
            } else {
                hmMethods.put(methodKey, methodEntity);
            }
        });

        ResultSetMetaData rsMetaData = rs.getMetaData();
        int columnCount = rsMetaData.getColumnCount();
        DataTableEntity dataTable = new DataTableEntity(columnCount);
        //获取字段名称，类型
        for (int i = 0; i < columnCount; i++) {
            String columnName = rsMetaData.getColumnName(i + 1);
            int columnType = rsMetaData.getColumnType(i + 1);
            dataTable.setColumnName(columnName, i);
            dataTable.setColumnType(columnType, i);
        }

        //处理ResultSet数据信息
        while (rs.next()) {
            result.add(toRow(rs, clazz, dataTable, hmMethods));
        }
        return result;
    }

    private static <T> T toRow(ResultSet rs, Class<T> clazz, DataTableEntity dataTable, HashMap<String, MethodEntity> hmMethods) throws NoSuchMethodException, SQLException, InstantiationException, IllegalAccessException {
        T row = clazz.newInstance();
        int nColumnCount = dataTable.getColumnCount();
        String[] strColumnNames = dataTable.getColumnNames();
        for (int i = 0; i < nColumnCount; i++) {
            //获取字段值
            Object objColumnValue = rs.getObject(strColumnNames[i]);
            //获取set方法名
            if (strColumnNames[i] != null) {
                //获取set方法名
                String strMethodKey = "SET" + strColumnNames[i].toUpperCase();
                //值和方法都不为空,这里方法名不为空即可,值可以为空的
                //判断字段的类型,方法名，参数类型
                MethodEntity methodEntity = hmMethods.get(strMethodKey);
                if (methodEntity != null) {
                    String methodName = methodEntity.getMethodName();
                    int repeatMethodNum = methodEntity.getRepeatMethodNum();
                    Class[] paramTypes = methodEntity.getMethodParamTypes();
                    Method method = clazz.getMethod(methodName, paramTypes);
                    //如果重载方法数 > 1，则判断是否有java.lang.IllegalArgumentException异常，循环处理
                    try {
                        //设置参数,实体对象，实体对象方法参数
                        method.invoke(row, new Object[]{objColumnValue});
                    } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException e) {
                        //处理重载方法
                        for (int j = 1; j < repeatMethodNum; j++) {
                            try {
                                Class[] repeatParamTypes = methodEntity.getRepeatMethodsParamTypes(j - 1);
                                method = clazz.getMethod(methodName, repeatParamTypes);
                                method.invoke(row, new Object[]{objColumnValue});
                                break;
                            } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException ex) {
                                throw new SqlUtilException(ex);
                            }
                        }
                    }
                }
            }
        }
        return row;
    }

    /**
     * 判断表是否存在
     *
     * @param connection 数据库连接
     * @param tableName  表名
     * @return Boolean
     */
    public static Boolean isTableExists(Connection connection, String tableName) {
        return isTableExists(connection, tableName, null);
    }

    /**
     * 判断表是否存在
     *
     * @param connection 数据库连接
     * @param tableName  表名
     * @param dbType     数据库雷西看那个
     * @return boolean
     */
    public static Boolean isTableExists(Connection connection, String tableName, DbType dbType) {
        boolean isTableExists = false;
        try {
            if (DbType.h2.equals(dbType) || DbType.oracle.equals(dbType)) tableName = tableName.toUpperCase();
            else tableName = tableName.toLowerCase();
            ResultSet rset = connection.getMetaData().getTables(null, null, tableName, null);
            if (rset.next()) isTableExists = true;
        } catch (SQLException e) {
            log.error(e.getMessage(), e);
        }
        return isTableExists;
    }
}
