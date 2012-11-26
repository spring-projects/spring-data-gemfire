/*
 * Copyright 2002-2012 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import java.io.Serializable;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * @author David Turanski
 *
 */
@SuppressWarnings("serial")
public class RemoteMethodInvocation implements Serializable {
	private final String methodName;
	private  String className;
	private final Serializable[] arguments;
	
	public RemoteMethodInvocation(Class<?> clazz, String methodName, Serializable... args) {
		this(methodName,args);
		Assert.notNull(clazz , "class cannot be null");
		this.className = clazz.getName();
	 
	}

	
	public RemoteMethodInvocation(String className, String methodName, Serializable... args) {
		this(methodName,args);
		Assert.isTrue(StringUtils.hasLength(className.trim()) , "class cannot be null or empty");
		this.className = className;
	}
	
	private RemoteMethodInvocation(String methodName, Serializable... args){
		Assert.isTrue(StringUtils.hasLength(methodName.trim()), "methodName cannot be null or empty");
		Assert.isTrue(StringUtils.hasLength(methodName.trim()), "methodName cannot be null or empty");
		this.methodName = methodName;	 
		this.arguments = args == null ? new Serializable[]{} : args;
	}
	
	public String getMethodName() {
		return methodName;
	}
	public String getClassName() {
		return className;
	}
	public Serializable[] getArguments() {
		return arguments;
	}
}
