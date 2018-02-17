/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.repository.sample;

import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.util.ObjectUtils;

/**
 * The Plant class is a very simple ADT modeling a plant for SDG Repository testing purposes.
 *
 * @author John Blum
 * @see org.springframework.data.annotation.Id
 * @see Region
 * @since 1.4.0
 */
@Region("Plants")
@SuppressWarnings("unused")
public class Plant {

	@Id
	private Long id;

	private String name;

	public Long getId() {
		return this.id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public boolean equals(final Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof Animal)) {
			return false;
		}

		Animal that = (Animal) obj;

		return ObjectUtils.nullSafeEquals(this.getId(), that.getId())
			&& ObjectUtils.nullSafeEquals(this.getName(), that.getName());
	}

	@Override
	public int hashCode() {

		int hashValue = 17;

		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getId());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getName());

		return hashValue;
	}

	@Override
	public String toString() {
		return String.format("{ @type = %1$s, id = %2$s, name = %3$s }", getClass().getSimpleName(), getId(), getName());
	}
}
