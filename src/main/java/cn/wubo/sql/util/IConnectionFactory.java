package cn.wubo.sql.util;

import java.sql.Connection;
import java.sql.SQLException;

public interface IConnectionFactory {

    Connection init(ConnectionParam connectionParam) throws SQLException;
}
