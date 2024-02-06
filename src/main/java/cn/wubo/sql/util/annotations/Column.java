package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.ColumnType;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface Column {
    //数据库列名
    String value();

    //数据库列描述
    String desc() default "";

    //数据库列类型
    ColumnType type() default ColumnType.VARCHAR;

    //字符类型长度
    int length() default 200;

    //数值类型精度
    int precision() default 18;

    //数值类型小数位数
    int scale() default 2;
}
