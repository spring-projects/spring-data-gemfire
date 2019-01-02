/*
<<<<<<< Updated upstream
 * Copyright 2002-2019 the original author or authors.
=======
 * Copyright 2002-2019 the original author or authors.
>>>>>>> Stashed changes
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
package org.springframework.data.gemfire.function.execution;

import java.lang.reflect.Method;
import java.util.Set;

import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * @author David Turanski
 * @author John Blum
 */
public class OnRegionFunctionProxyFactoryBean extends GemfireFunctionProxyFactoryBean {

	private final OnRegionExecutionMethodMetadata methodMetadata;

	/**
	 * @param serviceInterface the Service class interface specifying the operations to proxy.
	 * @param gemfireOnRegionOperations an {@link GemfireOnRegionOperations} instance
	 */
	public OnRegionFunctionProxyFactoryBean(Class<?> serviceInterface,
			GemfireOnRegionOperations gemfireOnRegionOperations) {

		super(serviceInterface, gemfireOnRegionOperations);

		this.methodMetadata = new OnRegionExecutionMethodMetadata(serviceInterface);
	}

	@Override
	protected Iterable<?> invokeFunction(Method method, Object[] args) {

		GemfireOnRegionOperations gemfireOnRegionOperations =
			(GemfireOnRegionOperations) getGemfireFunctionOperations();

		OnRegionMethodMetadata onRegionMethodMetadata = this.methodMetadata.getMethodMetadata(method);

		int filterArgPosition = onRegionMethodMetadata.getFilterArgPosition();

		String functionId = onRegionMethodMetadata.getFunctionId();

		Set<?> filter = null;

		// extract filter from args if necessary
		if (filterArgPosition >= 0) {
			filter = (Set<?>) args[filterArgPosition];
			args = ArrayUtils.remove(args, filterArgPosition);
		}

		return filter != null ? gemfireOnRegionOperations.execute(functionId, filter, args)
			: gemfireOnRegionOperations.execute(functionId, args);
	}
}
