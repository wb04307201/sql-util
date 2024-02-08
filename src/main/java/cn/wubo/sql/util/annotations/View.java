package cn.wubo.sql.util.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface View {
    boolean show() default true;
    int sort() default 100;
}
