package cn.wubo.sql.util.annotations;

public @interface Ds {

    String url() default "";

    String username() default "";

    String passowrd() default "";
}
