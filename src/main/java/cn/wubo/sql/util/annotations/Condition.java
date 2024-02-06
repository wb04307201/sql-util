package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.StatementCondition;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface Condition {
    StatementCondition value() default StatementCondition.EQ;
}
