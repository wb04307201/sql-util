package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.DataTableEntity;
import cn.wubo.sql.util.entity.MethodEntity;
import cn.wubo.sql.util.exception.ExecuteSqlUtilsException;
import com.alibaba.druid.DbType;
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
     * @param params     参数
     * @param <T>        泛型
     * @return List<T>
     */
    public static <T> List<T> executeQuery(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeQuery ...... sql:{} params:{}", sql, params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet())
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            return getResultMap(preparedStatement.executeQuery());
        } catch (SQLException | NoSuchMethodException | InstantiationException | IllegalAccessException |
                 InvocationTargetException e) {
            throw new ExecuteSqlUtilsException(e);
        }
    }

    public static <T> List<T> executeQuery(Connection connection, SQL sql) {
        return executeQuery(connection, sql.getParse(), sql.getParams());
    }

    public static <T> List<T> executeQuery(Connection connection, String sql) {
        return executeQuery(connection, sql, new HashMap<>());
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
            for (Map.Entry<Integer, Object> entry : params.entrySet())
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            return preparedStatement.executeUpdate();
        } catch (SQLException e) {
            throw new ExecuteSqlUtilsException(e);
        }
    }

    public static int executeUpdate(Connection connection, SQL sql) {
        return executeUpdate(connection, sql.getParse(), sql.getParams());
    }

    public static int executeUpdate(Connection connection, String sql) {
        return executeUpdate(connection, sql, new HashMap<>());
    }

    /**
     * 处理返回值
     *
     * @param rs  游标
     * @param <T> 泛型
     * @return List<T>
     */
    private static <T> List<T> getResultMap(ResultSet rs) throws NoSuchMethodException, InstantiationException, IllegalAccessException, SQLException, InvocationTargetException {
        log.debug("getResultMap ...... ");
        Class<T> clazz = new GenericUtils<T>().getClassType();
        if (Arrays.stream(clazz.getInterfaces()).anyMatch(item -> item.getName().equals("java.util.Map")))
            return result2Map(rs, clazz);
        else return result2Class(rs, clazz);
    }

    private static <T> List<T> result2Map(ResultSet rs, Class<T> clazz) throws SQLException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
        log.debug("getResultMap ...... ");
        List<T> result = new ArrayList<>();
        ResultSetMetaData rsmd = rs.getMetaData();
        int count = rsmd.getColumnCount();// 获取列的数量
        //列头
        String[] headers = new String[count];
        for (int i = 1; i <= count; i++)
            headers[i - 1] = rsmd.getColumnLabel(i);
        //数据
        while (rs.next()) {
            T row = clazz.newInstance();
            Method method = clazz.getDeclaredMethod("put");
            for (int i = 1; i <= count; i++)
                method.invoke(row, headers[i - 1], rs.getObject(i));
            result.add(row);
        }
        return result;
    }

    private static <T> List<T> result2Class(ResultSet rs, Class<T> clazz) throws SQLException, NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {
        List<T> result = new ArrayList<>();
        //获取实体中定义的方法
        HashMap<String, MethodEntity> hmMethods = new HashMap<>();
        Arrays.stream(clazz.getDeclaredMethods()).forEach(method -> {
            MethodEntity methodEntity = new MethodEntity();
            //方法的名称
            String methodName = method.getName();
            String methodKey = methodName.toUpperCase();
            //方法的参数
            Class<?>[] paramTypes = method.getParameterTypes();
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

    private static <T> T toRow(ResultSet rs, Class<T> clazz, DataTableEntity dataTable, HashMap<String, MethodEntity> hmMethods) throws NoSuchMethodException, SQLException, InstantiationException, IllegalAccessException, InvocationTargetException {
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
                    Class<?>[] paramTypes = methodEntity.getMethodParamTypes();
                    Method method = clazz.getMethod(methodName, paramTypes);
                    //如果重载方法数 > 1，则判断是否有java.lang.IllegalArgumentException异常，循环处理
                    try {
                        //设置参数,实体对象，实体对象方法参数
                        method.invoke(row, objColumnValue);
                    } catch (IllegalArgumentException e) {
                        log.error(e.getMessage(), e);
                        //处理重载方法
                        for (int j = 1; j < repeatMethodNum; j++) {
                            try {
                                Class<?>[] repeatParamTypes = methodEntity.getRepeatMethodsParamTypes(j - 1);
                                method = clazz.getMethod(methodName, repeatParamTypes);
                                method.invoke(row, objColumnValue);
                                break;
                            } catch (IllegalArgumentException ex) {
                                log.error(ex.getMessage(), ex);
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

    public static Boolean isTableExists(Connection connection, SQL sql) {
        String tableName = sql.getTable();
        DbType dbType = sql.getDbType();
        if ((DbType.h2.equals(dbType) || DbType.oracle.equals(dbType) || DbType.postgresql.equals(dbType) || DbType.db2.equals(dbType) || DbType.dm.equals(dbType)))
            tableName = tableName.toUpperCase();
        else tableName = tableName.toLowerCase();
        return isTableExists(connection, null, null, tableName, new String[]{"TABLE"});
    }
}
