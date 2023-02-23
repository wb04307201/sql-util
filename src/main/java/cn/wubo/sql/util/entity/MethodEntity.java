package cn.wubo.sql.util.entity;

import java.util.ArrayList;

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
     * 获取参数名称
     *
     * @return
     */
    public String getMethodName() {
        return methodName;
    }

    /**
     * 获取方法参数类型列表
     *
     * @return
     */
    public Class[] getMethodParamTypes() {
        return methodParamTypes;
    }

    /**
     * 设置参数名称
     *
     * @param string
     */
    public void setMethodName(String string) {
        methodName = string;
    }

    /**
     * 设置参数类型列表
     *
     * @param classes
     */
    public void setMethodParamTypes(Class[] classes) {
        methodParamTypes = classes;
    }

    /**
     * 获取重载方法个数
     *
     * @return
     */
    public int getRepeatMethodNum() {
        return repeatMethodNum;
    }

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
     * 设置重载方法个数
     *
     * @param i
     */
    public void setRepeatMethodNum(int i) {
        repeatMethodNum = i;
    }

    /**
     * 设置重载方法参数类型
     *
     * @param list
     */
    public void setRepeatMethodsParamTypes(ArrayList list) {
        repeatMethodsParamTypes = list;
    }

    /*
     *获取重载方法类型
     * @return
     */
    public ArrayList getRepeatMethodsParamTypes() {
        return repeatMethodsParamTypes;
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
