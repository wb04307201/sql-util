package cn.wubo.sql.util.entity;

public class DataTableEntity {
    //查询出的ReslutSet中的字段数量
    private int columnCount = 0;
    //字段名称数组
    private String[] columnNames;
    //字段类型数组
    private int[] columnTypes;

    // 默认构造器
    public DataTableEntity() {
        this(0);
    }

    //初始化构造器
    public DataTableEntity(int columnCount) {
        this.columnCount = columnCount;
        this.columnNames = new String[columnCount];
        this.columnTypes = new int[columnCount];
    }

    //获取字段数量
    public int getColumnCount() {
        return this.columnCount;
    }

    //获取字段名称数组
    public String[] getColumnNames() {
        return this.columnNames;
    }

    //获取第index个字段名称，如果index字段不存在，则抛出ArrayIndexOutOfBoundsException异常
    public String getColumnName(int index) {
        if (index <= this.columnCount) {
            return this.columnNames[index];
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    //设置字段名称数组
    public void setColumnNames(String[] columnNames) {
        this.columnNames = columnNames;
    }

    //设置第index个字段名称，如果index字段不存在，则抛出ArrayIndexOutOfBoundsException异常
    public void setColumnName(String columnName, int index) {
        if (index <= this.columnCount) {
            this.columnNames[index] = columnName;
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    //获取字段类型数组
    public int[] getColumnTypes() {
        return this.columnTypes;
    }

    //获取字段类型
    public int getColumnType(int index) {
        if (index <= this.columnCount) {
            return this.columnTypes[index];
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    //设置字段类型数组
    public void setColumnTypes(int[] columnTypes) {
        this.columnTypes = columnTypes;
    }

    //获取字段类型
    public void setColumnType(int columnType, int index) {
        if (index <= this.columnCount) {
            this.columnTypes[index] = columnType;
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }
}
