/*
 * Copyright 2012-2019 the original author or authors.
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
package org.springframework.data.gemfire.repository.sample;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceConstructor;
import org.springframework.data.annotation.Transient;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.util.ObjectUtils;

/**
 * The Person class models a person.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see java.io.Serializable
 */
@Region("simple")
@JsonIgnoreProperties("name")
public class Person implements Serializable {

	private static final long serialVersionUID = 508843183613325255L;

	public Address address;

	@Id
	public Long id;

	public String firstname;
	public String lastname;

	@PersistenceConstructor
	public Person() {
	}

	public Person(Long id) {
		this.id = id;
	}

	public Person(String firstName, String lastName) {
		this.firstname = firstName;
		this.lastname = lastName;
	}

	public Person(Long id, String firstname, String lastname) {
		this.id = id;
		this.firstname = firstname;
		this.lastname = lastname;
	}

	/**
	 * Gets the Person's address.
	 *
	 * @return the Address of the Person.
	 */
	public Address getAddress() {
		return address;
	}

	/**
	 * Returns the identifier (ID) of this Person.
	 *
	 * @return a Long value with the ID of this Person.
	 */
	public Long getId() {
		return id;
	}

	/**
	 * Gets this Person's first name.
	 *
	 * @return the first name of this Person.
	 */
	public String getFirstname() {
		return firstname;
	}

	/**
	 * Gets this Person's last name.
	 *
	 * @return the last name of this Person.
	 */
	public String getLastname() {
		return lastname;
	}

	/**
	 * Returns the Person's full name.
	 *
	 * @return the first and last name of the Person.
	 * @see #getFirstname()
	 * @see #getLastname()
	 */
	@Transient
	public String getName() {
		return String.format("%1$s %2$s", getFirstname(), getLastname());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof Person)) {
			return false;
		}

		Person that = (Person) obj;

		return (this.id != null && this.id.equals(that.id));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return ObjectUtils.nullSafeHashCode(this.id);
	}

	@Override
	public String toString() {
		return String.format("{ @type = %1$s, id = %2$d, name = %3$s }", getClass().getName(), id, getName());
	}

}
