package cn.wubo.sql.util.annotations;


import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@Documented
public @interface Table {
    String value();

    String desc() default "";

    Ds ds() default @Ds(url = "", username = "", passowrd = "");
}
