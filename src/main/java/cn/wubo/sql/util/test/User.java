package cn.wubo.sql.util.test;

import cn.wubo.sql.util.annotations.Column;
import cn.wubo.sql.util.annotations.Key;
import cn.wubo.sql.util.annotations.Table;
import cn.wubo.sql.util.annotations.View;
import cn.wubo.sql.util.enums.ColumnType;
import lombok.Data;

import java.util.Date;

@Data
@Table(value = "test_user", desc = "用户")
public class User {

    @Key
    @Column(value = "id")
    private String id;

    @Column(value = "user_name", desc = "用户名", type = ColumnType.VARCHAR, length = 20)
    private String userName;

    @Column(value = "department", desc = "部门", view = @View(width = 300))
    private String department;

    @Column(value = "birth", desc = "生日", type = ColumnType.DATE)
    private Date birth;

    @Column(value = "birth1")
    private Date birth1;

    @Column(value = "age", desc = "年龄", type = ColumnType.NUMBER, precision = 10, scale = 0)
    private Integer age;

    @Column(value = "age1")
    private Integer age1;
}
