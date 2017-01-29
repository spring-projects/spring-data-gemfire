/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.GemFireCheckedException;
import org.apache.geode.GemFireException;
import org.apache.geode.cache.Region;

/**
 * Callback interface for GemFire code. To be used with {@link GemfireTemplate}'s execution methods, often as anonymous
 * classes within a method implementation. A typical implementation will call Region.get/put/query to perform some
 * operations on stored objects.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.apache.geode.cache.Region
 */
public interface GemfireCallback<T> {

	/**
	 * Gets called by {@link GemfireTemplate#execute(GemfireCallback)}. Does not need to care about
	 * handling transactions or exceptions.
	 *
	 * Allows a result object created within the callback to be returned, i.e. a domain object
	 * or a collection of domain objects.
	 *
	 * A thrown custom RuntimeException is treated as an application exception: it gets propagated to
	 * the caller of the template.
	 *
	 * @param region the GemFire Cache Region upon which the operation of this callback will be performed.
	 * @return a result object, or <tt>null</tt> if no result.
	 * @throws GemFireCheckedException for checked Exceptions occurring in GemFire.
	 * @throws GemFireException for runtime Exceptions occurring in GemFire.
	 * @see org.springframework.data.gemfire.GemfireTemplate
	 * @see org.apache.geode.cache.Region
	 */
	T doInGemfire(Region<?,?> region) throws GemFireCheckedException, GemFireException;

}
