package cn.wubo.sql.util.test;

import cn.wubo.sql.util.annotations.Column;
import cn.wubo.sql.util.annotations.Id;
import cn.wubo.sql.util.annotations.Table;
import cn.wubo.sql.util.enums.ColumnType;

import java.util.Date;

@Table(value = "test_user", desc = "用户")
public class User {

    @Id
    @Column(value = "id")
    private String id;

    @Column(value = "user_name", type = ColumnType.VARCHAR, length = 20)
    private String userName;

    @Column(value = "department")
    private String department;

    @Column(value = "birth", type = ColumnType.DATE)
    private Date birth;

    @Column(value = "birth1")
    private Date birth1;

    @Column(value = "age", type = ColumnType.NUMBER, precision = 10, scale = 0)
    private Integer age;

    @Column(value = "age1")
    private Integer age1;
}
