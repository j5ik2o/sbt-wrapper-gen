package wrapper.${packageName}

trait ${simpleTypeName} {

    val underlying${simpleTypeName}: ${packageName}.${simpleTypeName}

<#list methods as method>
    <#if method.name?starts_with("get")>
    <#assign methodName = method.name?substring(3)?uncap_first>
    def ${methodName}(<#list method.parameterTypeDescs as parameter>${parameter.name}:<#if parameter.notNull>${parameter.parameterTypeDesc.simpleTypeName}<#else>Option[${parameter.typeName.simpleTypeName}]</#if><#if parameter_has_next>,</#if></#list>): <#if method.notNull>${method.returnTypeDesc.simpleTypeName}<#else>Option[${method.returnTypeDesc.simpleTypeName}]</#if> =
    <#if method.notNull>
        underlying${simpleTypeName}.${method.name}(<#list method.parameterTypeDescs as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>)
    <#else>
        Option(underlying${simpleTypeName}.${method.name}(<#list method.parameterTypeDescs as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>))
    </#if>

    <#elseif method.name?starts_with("set")>
    <#assign methodName = method.name?substring(3)?uncap_first + "_= ">
    def ${methodName}(<#list method.parameterTypeDescs as parameter>${parameter.name}:<#if parameter.notNull>${parameter.parameterTypeDesc.simpleTypeName}<#else>Option[${parameter.parameterTypeDesc.simpleTypeName}]</#if><#if parameter_has_next>,</#if></#list>): <#if method.notNull>${method.returnTypeDesc.simpleTypeName}<#else>${method.returnTypeDesc.simpleTypeName}</#if> =
        underlying${simpleTypeName}.${method.name}(<#list method.parameterTypeDescs as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>)

    <#else>
    def ${method.name}(<#list method.parameterTypeDescs as parameter>${parameter.name}:<#if parameter.notNull>${parameter.parameterTypeDesc.simpleTypeName}<#else>Option[${parameter.parameterTypeDesc.simpleTypeName}]</#if><#if parameter_has_next>,</#if></#list>): <#if method.notNull || method.returnTypeDesc.simpleTypeName == "Unit">${method.returnTypeDesc.simpleTypeName}<#else>Option[${method.returnTypeDesc.simpleTypeName}]</#if> =
    <#if method.notNull || method.returnTypeDesc.simpleTypeName == "Unit">
        underlying${simpleTypeName}.${method.name}(<#list method.parameterTypeDescs as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>)
    <#else>
        Option(underlying${simpleTypeName}.${method.name}(<#list method.parameterTypeDescs as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>))
    </#if>

    </#if>
</#list>

}
