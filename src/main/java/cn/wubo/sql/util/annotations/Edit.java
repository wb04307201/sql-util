package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.EditType;

public @interface Edit {
    boolean show() default true;

    EditType type() default EditType.TEXT;

    boolean notNull() default false;

    boolean readonly() default false;

    String placeholder() default "";

    Item[] items() default {};
}
