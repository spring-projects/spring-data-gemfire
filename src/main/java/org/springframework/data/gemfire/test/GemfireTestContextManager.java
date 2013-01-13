/*
 * Copyright 2002-2013 the original author or authors.
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
package org.springframework.data.gemfire.test;

import org.springframework.test.context.TestContextManager;

/**
 * @author David Turanski
 *
 */
public class GemfireTestContextManager extends TestContextManager {
	
	private final static String GEMFIRE_CONTEXT_LOADER_CLASSNAME = GemfireSmartContextLoader.class.getName();

	/**
	 * @param testClass
	 * @param defaultContextLoaderClassName
	 */
	public GemfireTestContextManager(Class<?> testClass, String defaultContextLoaderClassName) {
		super(testClass, defaultContextLoaderClassName);
		initializeTestContext();
	}

	/**
	 * 
	 */
	private void initializeTestContext() {
		registerTestExecutionListeners(new GemfireTestExecutionListener());
	}

	/**
	 * @param testClass
	 */
	public GemfireTestContextManager(Class<?> testClass) {
		super(testClass,GEMFIRE_CONTEXT_LOADER_CLASSNAME);
		initializeTestContext();
	}

}
