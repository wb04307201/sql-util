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

    private static final String MISS_ID = "未找到id为%s的类定义";

    /**
     * 根据提供的ID查看对应的表格模型。
     *
     * @param id 要查看的实体的唯一标识符。
     * @return 返回对应ID的表格模型，如果找不到则抛出异常。
     * @throws EntityWebException 如果提供的ID不存在于类映射中，则抛出此异常。
     */
    public TableModel view(String id) {
        // 检查类映射中是否包含指定ID的实体
        if (classMap.containsKey(id)) return EntityUtils.getTable(classMap.get(id));
        else throw new EntityWebException(String.format(MISS_ID, id));
    }

    /**
     * 根据给定的ID和类信息构建相关数据库表。
     * 该方法首先将ID和类映射存储到classMap中，然后基于该类获取对应的TableModel。
     * 使用TableModel中的数据源信息计算MD5值，检查连接池中是否已存在对应的连接配置。
     * 如果不存在，则初始化连接池配置。接着，检查数据库中是否已存在对应的表。
     * 如果表不存在，则创建该表。
     *
     * @param id 用于标识构建的唯一ID
     * @param clazz 需要构建数据库表的类
     */
    public void build(String id, Class<?> clazz) {
        // 将id和clazz映射存储到classMap中
        classMap.put(id, clazz);
        // 根据clazz获取TableModel
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 根据数据源信息计算MD5值
        String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
        // 检查连接池中是否已配置了对应的MD5值
        if (Boolean.FALSE.equals(MutilConnectionPool.check(md5)))
            // 如果未配置，则初始化连接池
            MutilConnectionPool.init(md5, tableModel.getDs().getUrl(), tableModel.getDs().getUsername(), tableModel.getDs().getPassword());
        // 创建SQL对象，用于后续的表存在性检查和表创建操作
        SQL sql = new SQL(clazz) {
        };
        // 检查数据库中是否已存在表
        if (Boolean.FALSE.equals(MutilConnectionPool.run(md5, sql::isTableExists)))
            // 如果表不存在，则执行创建表的操作
            MutilConnectionPool.run(md5, conn -> sql.create().parse().createTable(conn));
    }

    /**
     * 根据提供的ID和参数选择数据。
     *
     * @param id 用于查找对应实体类的ID，映射到实体类的配置信息。
     * @param params 包含查询条件的参数映射。其中"wheres"键用于指定查询条件。
     * @return 返回查询结果，其类型取决于查询的实体类。
     * @throws EntityWebException 如果提供的ID不在已知实体类映射中。
     */
    public Object select(String id, Map<String, Object> params) {
        // 检查id是否存在于classMap中
        if (classMap.containsKey(id)) {
            // 获取对应的类并创建TableModel
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);

            // 通过数据源信息计算MD5，用于标识查询的唯一性
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));

            // 执行查询操作
            return MutilConnectionPool.run(md5, conn -> ModelSqlUtils.selectSql(JSON.parseObject(JSONObject.toJSONString(params.getOrDefault("wheres", new HashMap<String, Object>())), clazz)).executeQuery(conn));
        } else {
            // 如果id不存在，抛出异常
            throw new EntityWebException(String.format(MISS_ID, id));
        }
    }

    /**
     * 保存数据的方法。
     * 根据提供的ID，在classMap中查找对应的类，然后根据该类生成对应的SQL语句，最后执行SQL语句保存数据。
     * @param id 对应的类的标识符。
     * @param params 要保存的数据参数，以Map形式提供。
     * @return 执行保存操作的结果，通常是一个Object，具体类型依赖于执行的SQL语句和数据库操作库。
     * @throws EntityWebException 如果提供的ID在classMap中找不到对应的类，则抛出此异常。
     */
    public Object save(String id, Map<String, Object> params) {
        // 检查classMap中是否包含指定id的类
        if (classMap.containsKey(id)) {
            // 根据id获取对应的类
            Class<?> clazz = classMap.get(id);
            // 通过EntityUtils获取该类对应的TableModel
            TableModel tableModel = EntityUtils.getTable(clazz);
            // 生成数据源字符串的MD5值，用作后续数据库操作的标识
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            // 利用MutilConnectionPool执行保存操作的SQL语句，并返回执行结果
            return MutilConnectionPool.run(md5, conn -> ModelSqlUtils.saveSql(JSON.parseObject(JSONObject.toJSONString(params), clazz)).executeUpdate(conn));
        } else throw new EntityWebException(String.format(MISS_ID, id)); // 如果找不到对应的类，则抛出异常
    }

    /**
     * 根据提供的ID和参数删除相关数据。
     * @param id 用于查找对应数据表的标识符。
     * @param params 包含要删除的数据行的映射，键为"data"，值为待删除的数据行列表。
     * @return 返回被删除的行数。
     * @throws EntityWebException 如果找不到对应的类或者数据库操作失败。
     */
    public Object delete(String id, Map<String, Object> params) {
        // 检查id是否存在于classMap中，如果存在，获取对应的Class对象
        if (classMap.containsKey(id)) {
            Class<?> clazz = classMap.get(id);
            // 通过Class对象获取TableModel，进一步获取数据源ds，并计算其md5作为数据库连接的标识
            TableModel tableModel = EntityUtils.getTable(clazz);
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));
            // 从连接池获取与数据源md5匹配的数据库连接
            try (Connection conn = MutilConnectionPool.getConnection(md5)) {
                conn.setAutoCommit(false);  // 设置连接为非自动提交模式
                // 从params中获取待删除的数据行列表，如果不存在则初始化为空列表
                List<Map<String, Object>> rows = (List<Map<String, Object>>) params.getOrDefault("data", new ArrayList<Map<String, Object>>());
                // 遍历待删除的数据行，生成并执行删除SQL
                rows.forEach(row -> ModelSqlUtils.deleteSql(JSON.parseObject(JSONObject.toJSONString(row), clazz)).executeUpdate(conn));
                conn.commit();  // 提交事务
                conn.setAutoCommit(true);  // 恢复连接的自动提交模式
                return rows.size();  // 返回被删除的行数
            } catch (SQLException e) {
                // 如果数据库操作出现异常，抛出EntityWebException
                throw new EntityWebException(e.getMessage(), e);
            }
        } else throw new EntityWebException(String.format(MISS_ID, id));  // 如果找不到对应的Class，抛出EntityWebException
    }

    /**
     * 根据ID从数据库中获取对象。
     * @param id 对象的唯一标识符。
     * @param params 查询时的参数映射，键值对形式。
     * @return 查询到的对象，其类型取决于id所对应的类。
     * @throws EntityWebException 如果类映射中不包含指定ID，或者查询过程中发生异常。
     */
    public Object getById(String id, Map<String, Object> params) {
        // 检查id是否存在于类映射中
        if (classMap.containsKey(id)) {
            // 获取id对应的类，并根据该类获取表模型
            Class<?> clazz = classMap.get(id);
            TableModel tableModel = EntityUtils.getTable(clazz);
            // 通过表模型的数据源字符串计算MD5，用于后续SQL的缓存标识
            String md5 = Arrays.toString(DigestUtils.md5Digest(tableModel.getDs().toString().getBytes()));

            try {
                // 构造针对该类的查询SQL
                SQL<?> sql = ModelSqlUtils.SQL(clazz.getConstructor().newInstance()).select();
                // 遍历列模型，筛选出键为true的列，并尝试使用参数中的值添加一个等价的查询条件
                tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().ifPresent(col -> params.entrySet().stream().filter(entry -> entry.getKey().equalsIgnoreCase(col.getFieldName())).findAny().ifPresent(entry -> sql.addWhereEQ(col.getColumnName(), entry.getValue())));
                // 使用计算出的MD5和查询SQL从数据库中执行查询，并返回结果的第0个元素
                return MutilConnectionPool.run(md5, conn -> sql.parse().executeQuery(conn)).get(0);
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                // 在构造SQL或执行查询过程中遇到的任何异常，都将抛出EntityWebException
                throw new EntityWebException(e.getMessage(), e);
            }
        } else {
            // 如果类映射中不包含指定的ID，则抛出异常
            throw new EntityWebException(String.format(MISS_ID, id));
        }
    }
}
