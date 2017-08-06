/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.schema;

import java.util.Collections;
import java.util.Set;

import org.apache.geode.cache.GemFireCache;
import org.springframework.context.ApplicationContext;

/**
 * The {@link SchemaObjectCollector} interface defines a contract for implementing objects to search for
 * and find all schema objects of a particular type in a given context.
 *
 * Implementations of this interface know how to inspect the given context and find all references
 * to the schema object instances of a particular type.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.context.ApplicationContext
 * @since 2.0.0
 */
public interface SchemaObjectCollector<T> {

	/**
	 * Collects all schema objects of type {@link T} declared in the given {@link ApplicationContext}.
	 *
	 * @param applicationContext Spring {@link ApplicationContext} from which to collect schema objects
	 * of type {@link T}.
	 * @return a {@link Set} of all schema objects of type {@link T} declared in the {@link ApplicationContext};
	 * returns an empty {@link Set} if no schema object of type {@link T} could be found.
	 * @see org.springframework.context.ApplicationContext
	 * @see java.lang.Iterable
	 */
	default Iterable<T> collectFrom(ApplicationContext applicationContext) {
		return Collections.emptySet();
	}

	/**
	 * Collects all schema objects of type {@link T} defined in the {@link GemFireCache}.
	 *
	 * @param gemfireCache {@link GemFireCache} from which to collect schema objects of type {@link T}.
	 * @return a {@link Set} of all schema objects of type {@link T} defined in the {@link GemFireCache};
	 * returns an empty {@link Set} if no schema object of type {@link T} could be found.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see java.lang.Iterable
	 */
	default Iterable<T> collectFrom(GemFireCache gemfireCache) {
		return Collections.emptySet();
	}
}
