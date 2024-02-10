package cn.wubo.sql.util.web;


import org.springframework.context.annotation.Import;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@Documented
@Import({EntityWebConfig.class})
public @interface EnableEntityWeb {

}
