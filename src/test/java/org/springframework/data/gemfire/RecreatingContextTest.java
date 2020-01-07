/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire;

import org.junit.After;
import org.junit.Before;

import org.springframework.context.support.GenericXmlApplicationContext;

/**
 * Simple testing class that creates the app context after each method.
 * Used to properly destroy the beans defined inside Spring.
 *
 * @author Costin Leau
 */
public abstract class RecreatingContextTest {

	protected GenericXmlApplicationContext applicationContext;

	protected abstract String location();

	protected  void configureContext() { }

	@Before
	public void createCtx() {
		applicationContext = new GenericXmlApplicationContext();
		configureContext();
		applicationContext.load(location());
		applicationContext.registerShutdownHook();
		applicationContext.refresh();
	}

	@After
	public void destroyCtx() {
		if (applicationContext != null)
			applicationContext.destroy();
	}
}
