package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.GenerationType;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface Key {
    GenerationType value() default GenerationType.UUID;
}
