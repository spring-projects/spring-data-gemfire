/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.GenericXmlApplicationContext;

/**
 * The RecreatingSpringApplicationContextTest class is an abstract base class that creates the app context after each method.
 * Used to properly destroy the beans defined inside Spring.
 * 
 * @author Costin Leau
 * @author John Blum
 */
public abstract class RecreatingSpringApplicationContextTest {

	protected GenericXmlApplicationContext applicationContext;

	@Before
	public void createContext() {
		applicationContext = configureContext(new GenericXmlApplicationContext());
		applicationContext.load(location());
		applicationContext.registerShutdownHook();
		applicationContext.refresh();
	}

	protected abstract String location();

	protected <T extends ConfigurableApplicationContext> T configureContext(T context){
		return context;
	}

	@After
	public void destroyContext() {
		if (applicationContext != null) {
			applicationContext.destroy();
		}
	}

}
