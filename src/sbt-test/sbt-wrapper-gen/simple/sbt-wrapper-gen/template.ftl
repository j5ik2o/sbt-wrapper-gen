package wrapper.${packageName}

trait ${simpleTypeName} {

    val underlying${simpleTypeName}: ${packageName}.${simpleTypeName}

<#list methods as method>
    def ${method.name}(<#list method.parameters as parameter>${parameter.name}:<#if parameter.notNull>${parameter.typeName.simpleTypeName}<#else>Option[${parameter.typeName.simpleTypeName}]</#if><#if parameter_has_next>,</#if></#list>): <#if method.notNull>${method.returnType.simpleTypeName}<#else>Option[${method.returnType.simpleTypeName}]</#if> =
        <#if method.notNull>
            underlying${simpleTypeName}.${method.name}(<#list method.parameters as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>)
        <#else>
            Option(underlying${simpleTypeName}.${method.name}(<#list method.parameters as parameter><#if parameter.notNull>${parameter.name}<#else>${parameter.name}.orNull</#if><#if parameter_has_next>,</#if></#list>))
        </#if>

</#list>

}