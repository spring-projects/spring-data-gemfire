/*
 * Copyright 2012 the original author or authors.
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
package org.springframework.data.gemfire.repository;

import java.io.Serializable;

import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * Simple value object to hold an entity alongside an external key the entity shall be stored under.
 * 
 * @author Oliver Gierke
 */
public final class Wrapper<T, KEY extends Serializable> {

	private final KEY key;
	private final T entity;

	/**
	 * The entity to handle as well as the key.
	 * 
	 * @param entity the application domain object/entity to wrap.
	 * @param key must not be {@literal null}.
	 */
	public Wrapper(T entity, KEY key) {
		Assert.notNull(key);

		this.entity = entity;
		this.key = key;
	}

	/**
	 * @return the key
	 */
	public KEY getKey() {
		return key;
	}

	/**
	 * @return the entity
	 */
	public T getEntity() {
		return entity;
	}

	/* 
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object value) {
		if (value == this) {
			return true;
		}

		if (!(value instanceof Wrapper)) {
			return false;
		}

		Wrapper<?, ?> that = (Wrapper<?, ?>) value;

		return (this.key.equals(that.key) && ObjectUtils.nullSafeEquals(this.entity, that.entity));
	}

	/* 
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int result = 17;

		result += 31 * key.hashCode();
		result += 31 * ObjectUtils.nullSafeHashCode(entity);

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return ObjectUtils.nullSafeToString(getEntity());
	}

}
