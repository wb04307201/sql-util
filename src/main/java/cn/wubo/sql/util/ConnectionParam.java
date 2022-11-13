package cn.wubo.sql.util;

import lombok.Data;

import java.io.Serializable;

@Data
public class ConnectionParam implements Serializable {

    private static final long serialVersionUID = 1L;

    private String driverClassName = "org.h2.Driver"; // 数据库连接驱动
    private String url = "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE"; // 数据库连接url
    private String user = "sa"; // 数据库连接user
    private String password = ""; // 数据库连接password
    private int minConnection; // 数据库连接池最小连接数,即连接池创建后的初始数量
    private int maxConnection; // 数据库连接池最大连接数
    private long timeoutValue; // 连接的最大空闲时间
    private long waitTime; // 取得连接的最大等待时间
    private int incrementalConnections = 5; //连接池自动增加连接的数量
    private String connections = "jdbc"; //连接池自动增加连接的数量

    public ConnectionParam(){
    }

    public ConnectionParam(String url,String user,String password){
        this.url = url;
        this.user = user;
        this.password = password;
    }
}
