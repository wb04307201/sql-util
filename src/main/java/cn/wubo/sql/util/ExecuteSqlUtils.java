package cn.wubo.sql.util;

import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.PagerUtils;
import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;

import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ExecuteSqlUtils {

    /**
     * 执行查询
     *
     * @param connection
     * @param sql
     * @param params
     * @return
     */
    public static List<Map<String, Object>> executeQuery(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeQuery ...... sql:{} params:{}",sql,params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return getResultMap(preparedStatement.executeQuery());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public static <T> List<T> executeQuery(Connection connection, String sql, Map<Integer, Object> params, Class<T> clasz) {
        log.debug("executeQuery ...... sql:{} params:{}",sql,params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return getResultMap(preparedStatement.executeQuery(), clasz);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 执行分页查询
     *
     * @param connection
     * @param sql
     * @param params
     * @param dbType
     * @param offset
     * @param count
     * @return
     */
    public static List<Map<String, Object>> executeQueryByPage(Connection connection, String sql, Map<Integer, Object> params, String dbType, int offset, int count) {
        return executeQuery(connection, PagerUtils.limit(sql, DbType.valueOf(dbType), offset, count), params);
    }

    public static <T> List<T> executeQueryByPage(Connection connection, String sql, Map<Integer, Object> params, String dbType, int offset, int count, Class<T> clasz) {
        return executeQuery(connection, PagerUtils.limit(sql, DbType.valueOf(dbType), offset, count), params, clasz);
    }

    public static int executeUpdate(Connection connection, String sql, Map<Integer, Object> params) {
        log.debug("executeUpdate ...... sql:{} params:{}",sql,params);
        try (PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            for (Map.Entry<Integer, Object> entry : params.entrySet()) {
                preparedStatement.setObject(entry.getKey(), entry.getValue());
            }
            return preparedStatement.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException(e);
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
     * @param rs
     * @return
     * @throws SQLException
     */
    public static <T> List<T> getResultMap(ResultSet rs, Class<T> clazz) throws SQLException {
        log.debug("getResultMap ...... class:{}",clazz.getName());
        List<T> result = new ArrayList<>();
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
            result.add(JSON.parseObject(JSON.toJSONString(row), clazz));
        }
        return result;
    }

    public static boolean isTableExists(Connection conn, String tableName, DbType dbType) {
        boolean isTableExists = false;
        try {
            if (DbType.h2.equals(dbType) || DbType.oracle.equals(dbType)) tableName = tableName.toUpperCase();
            else tableName = tableName.toLowerCase();
            ResultSet rset = conn.getMetaData().getTables(null, null, tableName, null);
            if (rset.next()) isTableExists = true;
        } catch (SQLException e) {
            log.error(e.getMessage(), e);
        }
        return isTableExists;
    }

    public static Boolean isTableExists(Connection conn, String tableName) {
        return isTableExists(conn, tableName, null);
    }
}
