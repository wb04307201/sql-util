<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>${data.name}-${data.desc}</title>
    <link rel="stylesheet" type="text/css" href="${contextPath}/jgradio/static/layui/2.9.4/css/layui.css"/>
    <script type="text/javascript" src="${contextPath}/jgradio/static/layui/2.9.4/layui.js"></script>
    <style>
        body {
            padding: 10px 20px 10px 20px;
        }
    </style>
</head>
<body>
<!-- 查询区域 -->
<form class="layui-form layui-row layui-col-space16">
    <#list data.cols as item>
        <#if item.getEdit().search>
            <div class="layui-col-md4">
                <div class="layui-form-item">
                    <label class="layui-form-label">${item.desc}</label>
                    <div class="layui-input-block">
                        <#if item.getEdit().type?? && item.getEdit().type == 'SELECT'>
                            <select name="${item.name}">
                                <option value="" selected>全部</option>
                                <#if item.getEdit().items?size gt 0>
                                    <#list item.getEdit().items as option>
                                        <option value="${option.value}">${option.label}</option>
                                    </#list>
                                </#if>
                            </select>
                        <#elseif item.getEdit().type?? && item.getEdit().type == 'NUMBER'>
                            <input type="text" name="${item.name}" placeholder="${item.getEdit().placeholder}"
                                   class="layui-input"
                                   lay-affix="number" step="${item.getEdit().step}" <#if item.getEdit().min??></#if>>
                        <#else>
                            <input type="text" name="${item.name}" placeholder="${item.getEdit().placeholder}"
                                   class="layui-input"
                                   lay-affix="clear">
                        </#if>
                    </div>
                </div>
            </div>
        </#if>
    </#list>
    <div class="layui-btn-container layui-col-xs12" style="text-align: right;">
        <button class="layui-btn" lay-submit lay-filter="table-search">查询</button>
        <button type="reset" class="layui-btn layui-btn-primary">重置</button>
    </div>
</form>
<!-- 原始容器 -->
<table class="layui-hide" id="table"></table>
<!-- 工具栏模板 -->
<script type="text/html" id="table-toolbar">
    <div class="layui-btn-container">
        <button class="layui-btn layui-btn-sm" lay-event="add">新建</button>
        <button class="layui-btn layui-btn-sm" lay-event="del">删除</button>
    </div>
</script>
<!-- 操作列 -->
<script type="text/html" id="table-templet-operator">
    <div class="layui-clear-space">
        <a class="layui-btn layui-btn-xs" lay-event="delete">删除</a>
        <a class="layui-btn layui-btn-xs" lay-event="edit">编辑</a>
    </div>
</script>
<!-- 弹出层 -->
<div id="edit-layer-wrapper" style="display: none;">
    <div class="layui-form" lay-filter="filter-edit-layer" style="margin: 16px;">
        <#list data.cols as item>
            <#if item.getEdit().show>
                <div class="layui-col-md4">
                    <div class="layui-form-item">
                        <label class="layui-form-label">${item.desc}</label>
                        <div class="layui-input-block">
                            <#if item.getEdit().type?? && item.getEdit().type == 'SELECT'>
                                <select name="${item.name}">
                                    <option value="" selected>全部</option>
                                    <#if item.getEdit().items?size gt 0>
                                        <#list item.getEdit().items as option>
                                            <option value="${option.value}">${option.label}</option>
                                        </#list>
                                    </#if>
                                </select>
                            <#elseif item.getEdit().type?? && item.getEdit().type == 'NUMBER'>
                                <input type="text" name="${item.name}" placeholder="${item.getEdit().getPlaceholder()}"
                                       class="layui-input"
                                       lay-affix="number"
                                       step="${item.getEdit().step}" <#if item.getEdit().min??></#if>>
                            <#else>
                                <input type="text" name="${item.name}" placeholder="${item.getEdit().getPlaceholder()}"
                                       class="layui-input"
                                       lay-affix="clear">
                            </#if>
                        </div>
                    </div>
                </div>
            </#if>
        </#list>
    </div>
</div>
<script>
    layui.use(['table', 'form', 'util'], function () {
        let table = layui.table, form = layui.form, layer = layui.layer, $ = layui.$;

        // 搜索提交
        form.on('submit(table-search)', function (data) {
            var field = data.field; // 获得表单字段
            // 执行搜索重载
            table.reload('table', {
                where: {wheres: field} // 搜索的字段
            });
            return false; // 阻止默认 form 跳转
        });

        table.render({
            elem: '#table',
            cols: [[ //标题栏
                {type: 'checkbox', fixed: 'left'},
                {type: 'numbers', fixed: 'left'},
                {field: 'id', title: 'ID', width: 150, fixed: 'left'},
                <#list data.cols as item>
                <#if item.getView().getShow()>
                {
                    field: '${item.name}',
                    title: '${item.desc}',
                    <#if item.getView().getWidth()??>width: ${item.getView().getWidth()}</#if>
                },
                </#if>
                </#list>
                {field: 'operator', title: '操作', width: 110, fixed: 'right', templet: '#table-templet-operator'},
            ]],
            where: {wheres: {}},
            url: '${contextPath}/jgradio/select/${id}',
            method: 'post',
            contentType: 'application/json',
            parseData: function (res) { // res 即为原始返回的数据
                return {
                    "code": res.code === 200 ? 0 : res.code, // 解析接口状态
                    "msg": res.message, // 解析提示文本
                    "count": res.data.length, // 解析数据长度
                    "data": res.data // 解析数据列表
                };
            },
            // height: 'full-155',
            toolbar: '#table-toolbar',
        });

        // 头部工具栏事件
        table.on('toolbar(table)', function (obj) {
            let options = obj.config; // 获取当前表格属性配置项
            let checkStatus = table.checkStatus(options.id); // 获取选中行相关数据

            // 根据不同的事件名进行相应的操作
            switch (obj.event) { // 对应模板元素中的 lay-event 属性值
                case 'add':
                    form.val('filter-edit-layer', {
                        <#list data.cols as item>
                        "${item.name}": "",
                        </#list>
                    });
                    openRow();
                    break;
                case 'del':
                    if (checkStatus.data.length === 0)
                        return layer.msg('请选择至少一行');
                    delRow(checkStatus.data);
                    break;
            }
        });

        // 操作列事件
        table.on('tool(table)', function (obj) {
            let data = obj.data; // 获得当前行数据
            switch (obj.event) {
                case 'edit':
                    editRow(data)
                    break;
                case 'delete':
                    delRow([data])
                    break;
            }
        })

        function openRow() {
            layer.open({
                type: 1, // page 层类型，其他类型详见「基础属性」
                title: '新增',
                offset: 'r',
                anim: 'slideLeft', // 从右往左
                area: ['80%', '100%'],
                content: $('#edit-layer-wrapper'),
                btn: ['保存', '取消'],
                btn1: function (index, layero, that) {
                    form.submit('filter-edit-layer', function (data) {
                        let field = data.field; // 获取表单全部字段值

                        fetch("${contextPath}/business/${id}/saveOrUpdateBatch", {
                            method: 'POST',
                            headers: {
                                'Content-Type': 'application/json'
                            },
                            body: JSON.stringify({data: [field]})
                        })
                            .then(response => response.json())
                            .then(res => {
                                if (res.code === 200) {
                                    form.val('filter-edit-layer', {
                                        <#list data.cols as item>
                                        "${item.name}": "",
                                        </#list>
                                    });
                                    table.reloadData('table', {});
                                    layer.close(index);
                                    layer.msg(res.message);
                                } else {
                                    layer.msg(res.message);
                                }
                            })
                            .catch(err => layer.msg(err))
                    });
                },
                btn2: function (index, layero, that) {
                },
            });
        }

        function delRow(rows) {
            for (let data of rows) {
                if (data.isActive === '1')
                    return layer.msg("已激活表不允许删除!");
            }
            layer.confirm('确定要删除么？', {icon: 3}, function (index, layero, that) {
                fetch("${contextPath}/business/${id}/removeBatchByIds", {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify({data: rows})
                })
                    .then(response => response.json())
                    .then(res => {
                        table.reloadData('table', {});
                        layer.close(index);
                        layer.msg(res.message);
                    })
                    .catch(err => {
                        layer.msg(err)
                        layer.close(index);
                    })
            }, function (index, layero, that) {
            });
        }

        function editRow(data) {
            if (data.isActive === '1')
                return layer.msg("已激活表不允许编辑!");
            form.val('filter-edit-layer', {
                <#list data.cols as item>
                "${item.name}": "",
                </#list>
            });
            fetch("${contextPath}/business/${id}/getById?id=" + data.id)
                .then(response => response.json())
                .then(res => {
                    if (res.code === 200) {
                        let data = res.data;
                        form.val('filter-edit-layer', data);
                        openRow();
                    } else {
                        layer.msg(res.message);
                    }
                })
                .catch(err => layer.msg(err))
        }
    })
</script>
</body>
</html>