package cn.wubo.sql.util.annotation;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface Column {
    String name();

    String desc() default "";

    String definition() default "";
}
