package cn.wubo.sql.util.impl;

import cn.wubo.sql.util.ConnectionParam;
import cn.wubo.sql.util.IConnectionFactory;
import com.alibaba.druid.pool.DruidDataSource;
import org.springframework.util.StringUtils;

import java.sql.Connection;
import java.sql.SQLException;

public class DruidImpl implements IConnectionFactory {

    private DruidDataSource initDataSource(ConnectionParam connectionParam) throws SQLException {
        try (DruidDataSource dataSource = new DruidDataSource()) {
            if (StringUtils.hasLength(connectionParam.getDriverClassName())) {
                dataSource.setDriverClassName(connectionParam.getDriverClassName());
            }
            dataSource.setValidationQuery("select 1");
            // 也可使用setTestWhileIdle(false)关闭检测
            dataSource.setUrl(connectionParam.getUrl());
            dataSource.setUsername(connectionParam.getUser());
            dataSource.setPassword(connectionParam.getPassword());
            // 以下参数采用默认值，如果发现哪个有问题，单独设置哪个
            dataSource.setMaxWait(6000);
            dataSource.init();
            return dataSource;
        }
    }

    @Override
    public Connection init(ConnectionParam connectionParam) throws SQLException {
        try (DruidDataSource druidDataSource = initDataSource(connectionParam);
             Connection conn = druidDataSource.getConnection()) {
            return conn;
        }
    }
}
