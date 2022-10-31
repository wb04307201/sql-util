package cn.wubo.sql.util;

import lombok.Data;

import java.sql.Connection;

@Data
public class PooledConnection {
    //数据库连接
    private Connection connection;
    //对象连接是否被使用
    private boolean busy = false;

    // 构造函数，根据一个 Connection 构告一个 PooledConnection 对象
    public PooledConnection(Connection connection) {
        this.connection = connection;
    }
}
