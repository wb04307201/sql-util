package cn.wubo.sql.util.web;

import cn.wubo.sql.util.ModelSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.exception.EntityWebException;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import org.springframework.util.DigestUtils;

import java.lang.reflect.InvocationTargetException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class EntityWebService {

    private static Map<String, Class<?>> classMap = new ConcurrentHashMap<>();

    public TableModel view(String id) {
        if (classMap.containsKey(id)) return EntityUtils.getTable(classMap.get(id));
        else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

    public void build(String id, Class<?> clazz) {
        classMap.put(id, clazz);
        TableModel tableModel = EntityUtils.getTable(clazz);
        String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
        if (Boolean.FALSE.equals(MutilConnectionPool.check(md5)))
            MutilConnectionPool.init(md5, tableModel.getDs().getUrl(), tableModel.getDs().getUsername(), tableModel.getDs().getPassword());
        SQL sql = new SQL(clazz) {
        };
        if (Boolean.FALSE.equals(MutilConnectionPool.run(md5, sql::isTableExists)))
            MutilConnectionPool.run(md5, conn -> sql.create().parse().createTable(conn));
    }

    public Object select(String id, Map<String, Object> params) {
        if (classMap.containsKey(id)) {
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            return MutilConnectionPool.run(md5, conn -> ModelSqlUtils.selectSql(JSON.parseObject(JSONObject.toJSONString(params.getOrDefault("wheres", new HashMap<String, Object>())), clazz)).executeQuery(conn));
        } else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

    public Object save(String id, Map<String, Object> params) {
        if (classMap.containsKey(id)) {
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            return MutilConnectionPool.run(md5, conn -> ModelSqlUtils.saveSql(JSON.parseObject(JSONObject.toJSONString(params), clazz)).executeUpdate(conn));
        } else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

    public Object delete(String id, Map<String, Object> params) {
        if (classMap.containsKey(id)) {
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            try (Connection conn = MutilConnectionPool.getConnection(md5)) {
                conn.setAutoCommit(false);
                List<Map<String, Object>> rows = (List<Map<String, Object>>) params.getOrDefault("data", new ArrayList<Map<String, Object>>());
                rows.forEach(row -> ModelSqlUtils.deleteSql(JSON.parseObject(JSONObject.toJSONString(row), clazz)).executeUpdate(conn));
                conn.commit();
                conn.setAutoCommit(true);
                return rows.size();
            } catch (SQLException e) {
                throw new EntityWebException(e.getMessage(), e);
            }
        } else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

    public Object getById(String id, Map<String, Object> params) {
        if (classMap.containsKey(id)) {
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            try {
                SQL<?> sql = ModelSqlUtils.SQL(clazz.getConstructor().newInstance()).select();
                tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().ifPresent(col -> params.entrySet().stream().filter(entry -> entry.getKey().equalsIgnoreCase(col.getFieldName())).findAny().ifPresent(entry -> sql.addWhereEQ(col.getColumnName(), entry.getValue())));
                return MutilConnectionPool.run(md5, conn -> sql.parse().executeQuery(conn)).get(0);
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new EntityWebException(e.getMessage(), e);
            }
        } else throw new EntityWebException("未找到id为" + id + "的类定义");
    }

}
