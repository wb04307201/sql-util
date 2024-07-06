import cn.wubo.sql.util.annotations.*;
import cn.wubo.sql.util.enums.ColumnType;
import cn.wubo.sql.util.enums.EditType;
import cn.wubo.sql.util.enums.StatementCondition;
import lombok.Data;

import java.util.Date;

@Data
@Table(value = "test_user", desc = "用户", ds = @Ds(url = "jdbc:h2:file:./data/demo;AUTO_SERVER=TRUE", username = "sa", passowrd = ""))
public class User {
    @Key
    @Column(value = "id")
    private String id;

    @Condition(value = StatementCondition.LIKE)
    @Column(value = "user_name", desc = "用户名", type = ColumnType.VARCHAR, length = 20, edit = @Edit(notNull = true))
    private String userName;

    @Column(value = "department", desc = "部门", view = @View(width = 300, translatable = true, items = {@Item(value = "1", label = "部门1"), @Item(value = "2", label = "部门2"), @Item(value = "3", label = "部门3")}), edit = @Edit(type = EditType.SELECT, items = {@Item(value = "1", label = "部门1"), @Item(value = "2", label = "部门2"), @Item(value = "3", label = "部门3")}))
    private String department;

    @Column(value = "birth", desc = "生日", type = ColumnType.DATE, edit = @Edit(type = EditType.DATE))
    private Date birth;

    @Condition(value = StatementCondition.LIKE)
    @Column(value = "age", desc = "年龄", type = ColumnType.NUMBER, precision = 10, scale = 0, edit = @Edit(type = EditType.NUMBER))
    private Integer age;

    @Condition(value = StatementCondition.LIKE)
    @Column(value = "amount", desc = "薪酬", type = ColumnType.NUMBER, precision = 10, scale = 2, edit = @Edit(type = EditType.NUMBER))
    private Float amount;

    @Column(value = "status", desc = "在职", type = ColumnType.VARCHAR, length = 1, view = @View(translatable = true, items = {@Item(value = "Y", label = "在职"), @Item(value = "N", label = "离职")}), edit = @Edit(type = EditType.CHECKBOX, items = {@Item(value = "Y", label = "在职"), @Item(value = "N", label = "离职")}))
    private String status;
}
