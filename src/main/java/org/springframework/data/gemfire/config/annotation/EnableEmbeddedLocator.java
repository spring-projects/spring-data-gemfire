/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;

/**
 * The EnableEmbeddedLocator annotation marks a Spring {@link org.springframework.context.annotation.Configuration}
 * class to embed a GemFire Locator service in the GemFire server-side data member node.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EmbeddedLocatorConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import(EmbeddedLocatorConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableEmbeddedLocator {

	/**
	 * If set, automatically starts a Locator in the current process when the member connects to the distributed system
	 * and stops the Locator when the member disconnects.
	 * To use, specify the Locator with an optional address or host specification and a required port number, in one of
	 * these formats:
	 *
	 * start-locator=address[port1]
	 *
	 * start-locator=port1
	 *
	 * If you only specify the port, the address assigned to the member is used for the Locator.
	 * If not already there, this locator is automatically added to the list of Locators in this
	 * set of GemFire Properties.
	 *
	 * Defaults to {@literal 10334} (The default GemFire Locator port).
	 */
	String startLocator() default "localhost[10334]";

}
