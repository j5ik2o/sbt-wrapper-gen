package wrapper.${packageName}

trait ${simpleTypeName} {

    val underlying: ${packageName}.${simpleTypeName}

<#list methods as method>
    def ${method.name}(<#list method.parameters as parameter>${parameter.name}: ${parameter.typeName.simpleTypeName}<#if parameter_has_next>,</#if></#list>): ${method.returnType.simpleTypeName} =
        underlying.${method.name}(<#list method.parameters as parameter>${parameter.name}<#if parameter_has_next>,</#if></#list>)

</#list>

}