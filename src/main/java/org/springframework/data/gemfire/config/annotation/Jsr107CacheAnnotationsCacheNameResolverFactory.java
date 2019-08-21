/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import java.util.Collections;
import java.util.Set;

import org.springframework.util.ClassUtils;

/**
 * Factory class used to construct an instance of the {@link Jsr107CacheAnnotationsCacheNameResolver} if and only if
 * the JSR-107, JCache API lib is on the application classpath.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.Jsr107CacheAnnotationsCacheNameResolver
 * @since 2.2.0
 */
class Jsr107CacheAnnotationsCacheNameResolverFactory {

	private final boolean jcacheApiPresent;

	Jsr107CacheAnnotationsCacheNameResolverFactory() {

		this.jcacheApiPresent =
			ClassUtils.isPresent("javax.cache.annotation.CacheResult", getClass().getClassLoader());
	}

	CachingDefinedRegionsConfiguration.CacheNameResolver create() {

		return this.jcacheApiPresent
			? new Jsr107CacheAnnotationsCacheNameResolver()
			: new NoOpCacheNameResolver();
	}

	private static class NoOpCacheNameResolver implements CachingDefinedRegionsConfiguration.CacheNameResolver {

		@Override
		public Set<String> resolveCacheNames(Class<?> type) {
			return Collections.emptySet();
		}
	}
}
