package cn.wubo.sql.util;

import cn.wubo.sql.util.impl.DruidImpl;
import cn.wubo.sql.util.impl.JdbcImpl;
import com.alibaba.druid.DbType;
import lombok.extern.slf4j.Slf4j;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Optional;
import java.util.Vector;

/**
 * 1.初始化连接时，默认使用了一个h2数据库
 * private static ConnectionPool connectionPool = new ConnectionPool(new ConnectionParam());
 * 2.获取一个connection
 * Connection conn = connectionPool.getConnection();
 * 3.如果需要事务，使用setAutoCommit关闭自动提交，并使用commit手动提交到数据库
 * conn.setAutoCommit(false);
 * conn.commit();
 * 4.最后需要将connection放回连接池
 * connectionPool.returnConnection(conn);
 */
@Slf4j
public class ConnectionPool {

    private ConnectionParam param;
    private IConnectionFactory connectionFactory;

    public ConnectionPool(ConnectionParam param) {
        this.param = param;
        connectionFactory = param.getConnections().equals("druid") ? new DruidImpl() : new JdbcImpl();
    }

    public DbType getDbType(){
        String className = this.param.getDriverClassName();
        switch (className){
            case "org.h2.Driver":return DbType.h2;
            case "oracle.jdbc.driver.OracleDriver":return DbType.oracle;
            case "com.mysql.jdbc.Driver":
            default:
                return DbType.mysql;
        }
    }

    public Vector<PooledConnection> connections = new Vector<>();

    public synchronized Connection getConnection() throws SQLException, InterruptedException {
        log.debug("获取数据库连接 ......");
        Connection conn = getFreeConnection(); // 获得一个可用的数据库连接
        // 如果目前没有可以使用的连接，即所有的连接都在使用中
        while (conn == null) {
            // 等一会再试
            wait(250);
            conn = getFreeConnection(); // 重新再试，直到获得可用的连接，如果
            // getFreeConnection() 返回的为 null
            // 则表明创建一批连接后也不可获得可用连接
        }
        return conn;// 返回获得的可用的连接
    }

    private Connection getFreeConnection() throws SQLException {
        log.debug("获取空闲数据库连接 ......");
        // 从连接池中获得一个可用的数据库连接
        Connection conn = findFreeConnection();
        if (conn == null) {
            // 如果目前连接池中没有可用的连接
            // 创建一些连接,但如果已经达到最大，则不在创建
            createConnections(this.param.getIncrementalConnections());
            // 重新从池中查找是否有可用连接
            conn = findFreeConnection();
            if (conn == null) {
                // 如果创建连接后仍获得不到可用的连接，则返回 null
                return null;
            }
        }
        return conn;
    }

    private Connection findFreeConnection() {
        log.debug("查找空闲数据库连接 ......");
        Optional<PooledConnection> optional = connections.stream().filter(ele -> !ele.isBusy()).findAny();
        if (optional.isPresent()) {
            PooledConnection pConn = optional.get();
            pConn.setBusy(true);
            return pConn.getConnection();
        } else return null;
    }

    private void createConnections(int numConnections) throws SQLException {
        // 循环创建指定数目的数据库连接
        for (int x = 0; x < numConnections; x++) {
            // 是否连接池中的数据库连接的数量己经达到最大？最大值由类成员 maxConnections 控制，如果 maxConnections 为0或负数，表示连接数量没有限制。
            // 如果连接数己经达到最大，即退出。
            if (this.param.getMaxConnection() > 0 && connections.size() >= this.param.getMaxConnection()) {
                break;
            }
            // 增加一个连接到连接池中（向量 connections 中）
            try {
                connections.addElement(new PooledConnection(connectionFactory.init(param)));
            } catch (SQLException e) {
                log.error("创建数据库连接失败！ " + e.getMessage());
                throw new SQLException();
            }
            log.debug("数据库连接己创建 ......");
        }
    }

    public void returnConnection(Connection conn) {
        log.debug("返回数据库连接 ......");
        connections.stream()
                .filter(ele -> conn == ele.getConnection())
                .findAny()
                .ifPresent(ele -> ele.setBusy(false));

    }
}
