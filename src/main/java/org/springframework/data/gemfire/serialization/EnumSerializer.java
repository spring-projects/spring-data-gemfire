/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.serialization;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.gemstone.gemfire.DataSerializer;
import com.gemstone.gemfire.internal.InternalDataSerializer;

/**
 * Generic Serializer for JDK Enums. The class needs to be registered only once - custom enums
 * will be then understood by the converter by calling {@link #addEnum(Class)}.
 * 
 * @author Costin Leau
 */
public class EnumSerializer extends DataSerializer implements Serializable {

	private static final long serialVersionUID = -7069461993489626976L;

	private static final ConcurrentMap<Class<?>, Enum[]> supportedClasses = new ConcurrentHashMap<Class<?>, Enum[]>();

	private int id = 1024;

	@Override
	public boolean toData(Object o, DataOutput out) throws IOException {
		if (o instanceof Enum<?>) {
			// add enum to the set
			Enum<?> enm = (Enum<?>) o;
			Class<?> cls = enm.getDeclaringClass();
			addEnum(cls);
			DataSerializer.writeClass(cls, out);
			out.writeInt(enm.ordinal());
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object fromData(DataInput in) throws IOException, ClassNotFoundException {
		Class cls = DataSerializer.readClass(in);
		if (cls.isEnum()) {
			addEnum(cls);
			int ordinal = in.readInt();
			return supportedClasses.get(cls)[ordinal];
		}
		throw new IOException("Non-enum class read from the stream -" + cls);
	}

	@SuppressWarnings("unchecked")
	public void addEnum(Class enumClass) {
		if (!supportedClasses.containsKey(enumClass)) {
			supportedClasses.put(enumClass, (Enum[]) enumClass.getEnumConstants());
		}

		// if registered, re-register the serializer to propagate the changes
		if (InternalDataSerializer.getSerializer(getId()) != null) {
			if (InternalDataSerializer.getSerializer(enumClass) == null) {
				InternalDataSerializer.unregister(getId());
				InternalDataSerializer.register(getClass());
			}
		}
	}

	@Override
	public Class<?>[] getSupportedClasses() {
		return supportedClasses.keySet().toArray(new Class<?>[supportedClasses.size()]);
	}

	@Override
	public int getId() {
		return id;
	}

	/**
	 * Sets the id for this serializer. Default is 1024;
	 * 
	 * @param id the id to set
	 */
	public void setId(int id) {
		this.id = id;
	}
}