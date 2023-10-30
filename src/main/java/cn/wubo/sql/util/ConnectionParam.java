package cn.wubo.sql.util;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ConnectionParam implements Serializable {

    private String url = "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE"; // 数据库连接url
    private String user = "sa"; // 数据库连接user
    private String password = ""; // 数据库连接password
    private int minConnection; // 数据库连接池最小连接数,即连接池创建后的初始数量
    private int maxConnection; // 数据库连接池最大连接数
    private int retryCount; // 获取数据库连接失败重试次数
    private long retryWaitTime = 250; // 获取数据库连接失败重试等待时间
    private int incrementalConnections = 5; // 连接池自动增加连接的数量
    private long validationTime = 600; // 连接是否有效检测时间
    private String validationQuery = "select 1"; //连接是否有效查询sql

    public ConnectionParam(String url, String user, String password) {
        this.url = url;
        this.user = user;
        this.password = password;
    }
}
