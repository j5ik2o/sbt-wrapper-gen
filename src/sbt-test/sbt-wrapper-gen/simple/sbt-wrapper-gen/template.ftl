package wrapper.${packageName}

trait ${simpleTypeName} {

    val underlying: ${packageName}.${simpleTypeName}

<#list methods as method>
    def ${method.name}(<#list method.parameters as parameter>${parameter.name}: ${parameter.typeName.simpleTypeName}<#if parameter_has_next>,</#if></#list>): <#if method.notNull>${method.returnType.simpleTypeName}<#else>Option[${method.returnType.simpleTypeName}]</#if> =
        <#if method.notNull>
            underlying.${method.name}(<#list method.parameters as parameter>${parameter.name}<#if parameter_has_next>,</#if></#list>)
        <#else>
            Option(underlying.${method.name}(<#list method.parameters as parameter>${parameter.name}<#if parameter_has_next>,</#if></#list>))
        </#if>

</#list>

}