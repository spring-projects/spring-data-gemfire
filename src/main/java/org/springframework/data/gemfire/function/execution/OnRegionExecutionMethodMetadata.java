/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

import java.lang.reflect.Method;
import java.util.Set;

import org.springframework.data.gemfire.function.GemfireFunctionUtils;
import org.springframework.data.gemfire.function.annotation.Filter;



/**
 * @author David Turanski
 *
 */
class OnRegionExecutionMethodMetadata extends FunctionExecutionMethodMetadata<OnRegionMethodMetadata>  {

	/**
	 * @param serviceInterface
	 */
	public OnRegionExecutionMethodMetadata(Class<?> serviceInterface) {
		super(serviceInterface);
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.FunctionExecutionMethodMetadata#newMetadataInstance(java.lang.reflect.Method)
	 */
	@Override
	protected OnRegionMethodMetadata newMetadataInstance(Method method) {
		return new OnRegionMethodMetadata(method);
	}

}

class OnRegionMethodMetadata extends MethodMetadata {

	private final int filterArgPosition;

	public OnRegionMethodMetadata(Method method) {
		super(method);
		this.filterArgPosition = GemfireFunctionUtils.getAnnotationParameterPosition(method, Filter.class, new Class<?>[]{Set.class});
	}

	public int getFilterArgPosition() {
		return this.filterArgPosition;
	}
}
