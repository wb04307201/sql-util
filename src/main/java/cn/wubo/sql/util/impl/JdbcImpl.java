package cn.wubo.sql.util.impl;

import cn.wubo.sql.util.ConnectionParam;
import cn.wubo.sql.util.IConnectionFactory;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class JdbcImpl implements IConnectionFactory {
    @Override
    public Connection init(ConnectionParam connectionParam) throws SQLException {
        return DriverManager.getConnection(connectionParam.getUrl(),connectionParam.getUser(),connectionParam.getPassword());
    }
}
