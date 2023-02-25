package cn.wubo.sql.util.entity;

import lombok.Data;

import java.util.ArrayList;

@Data
public class MethodEntity {
    //方法名称
    private String methodName;
    //重载方法个数
    private int repeatMethodNum = 1;
    //方法参数类型列表
    private Class[] methodParamTypes;
    //存放重载方法参数
    private ArrayList repeatMethodsParamTypes;

    /**
     * 获取第i个重载方法参数列表
     *
     * @return
     */
    public Class[] getRepeatMethodsParamTypes(int i) {
        int count = this.repeatMethodsParamTypes.size();
        if (i <= count) {
            return (Class[]) this.repeatMethodsParamTypes.get(i);
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    /**
     * 设置重载方法参数类型列表
     *
     * @param paramTypes
     */
    public void setRepeatMethodsParamTypes(Class[] paramTypes) {
        if (this.repeatMethodsParamTypes == null) {
            this.repeatMethodsParamTypes = new ArrayList();
        }
        repeatMethodsParamTypes.add(paramTypes);
    }
}
