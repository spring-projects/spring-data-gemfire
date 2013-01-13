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

package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.util.ObjectSizer;

/**
 * Simple utility class used for defining nested factory-method like definitions w/o polluting the container with useless beans.
 * 
 * @author Costin Leau
 */
class EvictionAttributesFactoryBean implements FactoryBean<EvictionAttributes>, InitializingBean {

	private EvictionAttributes evictionAttr;

	private Integer threshold = null;
	private ObjectSizer objectSizer = null;
	private EvictionAction action = null;
	private EvictionType type = EvictionType.ENTRY_COUNT;

	public void afterPropertiesSet() {
		if (action == null) {
			action = EvictionAction.DEFAULT_EVICTION_ACTION;
		}

		evictionAttr = createAttributes();
	}

	private EvictionAttributes createAttributes() {
		switch (type) {
		case HEAP_PERCENTAGE:
			return EvictionAttributes.createLRUHeapAttributes(objectSizer, action);

		case MEMORY_SIZE:
			if (threshold != null) {
				return EvictionAttributes.createLRUMemoryAttributes(threshold, objectSizer, action);
			}
			return EvictionAttributes.createLRUMemoryAttributes(objectSizer, action);

		// consider entry count as default
		case ENTRY_COUNT:
		default:
			if (threshold != null) {
				return EvictionAttributes.createLRUEntryAttributes(threshold, action);
			}
			return EvictionAttributes.createLRUEntryAttributes();
		}
	}

	public EvictionAttributes getObject() {
		return evictionAttr;
	}

	public Class<?> getObjectType() {
		return (evictionAttr != null ? evictionAttr.getClass() : EvictionAttributes.class);
	}

	public boolean isSingleton() {
		return true;
	}

	/**
	 * @return the threshold
	 */
	public Integer getThreshold() {
		return threshold;
	}

	/**
	 * @param threshold the threshold to set
	 */
	public void setThreshold(Integer threshold) {
		this.threshold = threshold;
	}

	/**
	 * @return the objectSizer
	 */
	public ObjectSizer getObjectSizer() {
		return objectSizer;
	}

	/**
	 * @param objectSizer the objectSizer to set
	 */
	public void setObjectSizer(ObjectSizer objectSizer) {
		this.objectSizer = objectSizer;
	}

	/**
	 * @return the action
	 */
	public EvictionAction getAction() {
		return action;
	}

	/**
	 * @param action the action to set
	 */
	public void setAction(EvictionAction action) {
		this.action = action;
	}

	/**
	 * @return the type
	 */
	public EvictionType getType() {
		return type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(EvictionType type) {
		this.type = type;
	}
}