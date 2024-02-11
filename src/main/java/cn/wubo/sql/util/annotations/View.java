package cn.wubo.sql.util.annotations;

public @interface View {
    boolean show() default true;

    boolean sortable() default true;

    boolean exportable() default true;

    int width() default 200;

    boolean translatable() default false;

    Item[] items() default {};
}
